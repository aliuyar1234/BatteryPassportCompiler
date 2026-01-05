{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Rules.Eval - Rule DSL Evaluator
--
-- Pure expression evaluation with memoization following
-- topological field ordering.
module BPC.Core.Rules.Eval
  ( -- * Evaluation
    evalExpr
  , evalFields
  , evalModule

    -- * Context
  , EvalContext (..)
  , emptyContext
  , withFacts
  , withFields

    -- * Values
  , Value (..)

    -- * Results
  , EvalResult
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import BPC.Core.Rules.AST
import BPC.Core.Rules.Error (EvalError (..))
import BPC.Core.Rules.Graph (buildGraph, topoSortFields)

-- | Runtime value representation.
data Value
  = VBool !Bool
  | VInt !Integer
  | VDec !Int !Integer          -- scale, value
  | VQty !Int !Integer !Text    -- scale, value, unit
  | VString !Text
  | VDate !Text
  | VNone
  | VSome !Value
  | VList ![Value]
  | VRecord !(Map Text Value)
  deriving stock (Eq, Show)

-- | Evaluation context.
data EvalContext = EvalContext
  { ctxFacts :: !(Map (Text, Text) Value)   -- ^ (factType, factKey) -> Value
  , ctxFields :: !(Map Text Value)          -- ^ field path -> computed Value
  , ctxVars :: !(Map Text Value)            -- ^ local variable bindings
  }
  deriving stock (Eq, Show)

-- | Evaluation result.
type EvalResult = Either EvalError

-- | Empty evaluation context.
emptyContext :: EvalContext
emptyContext = EvalContext Map.empty Map.empty Map.empty

-- | Add facts to context.
withFacts :: Map (Text, Text) Value -> EvalContext -> EvalContext
withFacts facts ctx = ctx { ctxFacts = facts `Map.union` ctxFacts ctx }

-- | Add computed fields to context.
withFields :: Map Text Value -> EvalContext -> EvalContext
withFields fields ctx = ctx { ctxFields = fields `Map.union` ctxFields ctx }

-- | Add a local variable binding.
withVar :: Text -> Value -> EvalContext -> EvalContext
withVar name val ctx = ctx { ctxVars = Map.insert name val (ctxVars ctx) }

-- | Evaluate an entire module, computing all fields in topological order.
evalModule :: EvalContext -> Module -> EvalResult (Map Text Value)
evalModule ctx modul = do
  let graph = buildGraph modul
  order <- case topoSortFields graph of
    Left cycleErr -> Left $ AssertionFailed (T.pack $ show cycleErr) "CYCLE"
    Right o -> Right o
  evalFields ctx (getFieldDecls modul) order
  where
    getFieldDecls (Module decls) = [fd | DeclField fd <- decls]

-- | Evaluate fields in topological order with memoization.
evalFields :: EvalContext -> [FieldDecl] -> [Text] -> EvalResult (Map Text Value)
evalFields ctx decls order = go ctx order Map.empty
  where
    declMap = Map.fromList [(unFieldPath (fieldName fd), fd) | fd <- decls]

    go :: EvalContext -> [Text] -> Map Text Value -> EvalResult (Map Text Value)
    go _ [] results = Right results
    go context (fieldName':rest) results =
      case Map.lookup fieldName' declMap of
        Nothing -> go context rest results  -- Skip non-field declarations
        Just fd -> do
          value <- evalExpr context (fieldExpr fd)
          let context' = context { ctxFields = Map.insert fieldName' value (ctxFields context) }
          go context' rest (Map.insert fieldName' value results)

-- | Evaluate an untyped expression in a context.
evalExpr :: EvalContext -> UntypedExpr -> EvalResult Value
evalExpr _ (ULitBool b) = Right $ VBool b
evalExpr _ (ULitInt n) = Right $ VInt n
evalExpr _ (ULitString s) = Right $ VString s
evalExpr _ (ULitDec scale val) = Right $ VDec scale val
evalExpr _ (ULitQty val _scale unit) = Right $ VQty 6 val unit

evalExpr ctx (UVar (Identifier name)) =
  case Map.lookup name (ctxVars ctx) of
    Just v -> Right v
    Nothing -> Left $ FactNotFound "variable" name

evalExpr ctx (UFieldRef (FieldPath path)) =
  case Map.lookup path (ctxFields ctx) of
    Just v -> Right v
    Nothing -> Left $ FactNotFound "field" path

evalExpr ctx (UBinOp op left right) = do
  leftVal <- evalExpr ctx left
  rightVal <- evalExpr ctx right
  evalBinOp op leftVal rightVal

evalExpr ctx (UUnaryOp op expr) = do
  val <- evalExpr ctx expr
  evalUnaryOp op val

evalExpr ctx (UFuncCall (Identifier name) args) = do
  argVals <- mapM (evalExpr ctx) args
  evalBuiltin ctx name argVals

evalExpr ctx (ULet (Identifier name) value body) = do
  val <- evalExpr ctx value
  let ctx' = withVar name val ctx
  evalExpr ctx' body

evalExpr ctx (UIf cond thenBranch elseBranch) = do
  condVal <- evalExpr ctx cond
  case condVal of
    VBool True -> evalExpr ctx thenBranch
    VBool False -> evalExpr ctx elseBranch
    _ -> Left $ AssertionFailed "if condition must be boolean" "TYPE_ERROR"

evalExpr ctx (UAssert cond errorCode message) = do
  condVal <- evalExpr ctx cond
  case condVal of
    VBool True -> Right $ VBool True
    VBool False -> Left $ AssertionFailed message errorCode
    _ -> Left $ AssertionFailed "assert condition must be boolean" "TYPE_ERROR"

-- | Evaluate binary operators.
evalBinOp :: Text -> Value -> Value -> EvalResult Value
evalBinOp "||" (VBool a) (VBool b) = Right $ VBool (a || b)
evalBinOp "&&" (VBool a) (VBool b) = Right $ VBool (a && b)
evalBinOp "==" a b = Right $ VBool (a == b)
evalBinOp "!=" a b = Right $ VBool (a /= b)
evalBinOp "<" (VInt a) (VInt b) = Right $ VBool (a < b)
evalBinOp ">" (VInt a) (VInt b) = Right $ VBool (a > b)
evalBinOp "<=" (VInt a) (VInt b) = Right $ VBool (a <= b)
evalBinOp ">=" (VInt a) (VInt b) = Right $ VBool (a >= b)
evalBinOp "+" (VInt a) (VInt b) = Right $ VInt (a + b)
evalBinOp "-" (VInt a) (VInt b) = Right $ VInt (a - b)
evalBinOp "*" (VInt a) (VInt b) = Right $ VInt (a * b)
evalBinOp "/" (VInt _) (VInt 0) = Left $ DivisionByZero "integer division"
evalBinOp "/" (VInt a) (VInt b) = Right $ VInt (a `div` b)
evalBinOp "+" (VDec s1 a) (VDec s2 b)
  | s1 == s2 = Right $ VDec s1 (a + b)
  | otherwise = Left $ AssertionFailed "Dec scale mismatch" "TYPE_ERROR"
evalBinOp "-" (VDec s1 a) (VDec s2 b)
  | s1 == s2 = Right $ VDec s1 (a - b)
  | otherwise = Left $ AssertionFailed "Dec scale mismatch" "TYPE_ERROR"
evalBinOp "+" (VQty s1 a u1) (VQty s2 b u2)
  | u1 == u2 && s1 == s2 = Right $ VQty s1 (a + b) u1
  | u1 /= u2 = Left $ AssertionFailed ("Unit mismatch: " <> u1 <> " vs " <> u2) "UNIT_MISMATCH"
  | otherwise = Left $ AssertionFailed "Qty scale mismatch" "TYPE_ERROR"
evalBinOp op _ _ = Left $ AssertionFailed ("Unknown operator: " <> op) "TYPE_ERROR"

-- | Evaluate unary operators.
evalUnaryOp :: Text -> Value -> EvalResult Value
evalUnaryOp "!" (VBool b) = Right $ VBool (not b)
evalUnaryOp "-" (VInt n) = Right $ VInt (negate n)
evalUnaryOp "-" (VDec s v) = Right $ VDec s (negate v)
evalUnaryOp op _ = Left $ AssertionFailed ("Unknown unary operator: " <> op) "TYPE_ERROR"

-- | Evaluate built-in functions.
evalBuiltin :: EvalContext -> Text -> [Value] -> EvalResult Value
evalBuiltin ctx "getFact" [VString factType, VString factKey] =
  case Map.lookup (factType, factKey) (ctxFacts ctx) of
    Just v -> Right $ VSome v
    Nothing -> Right VNone
evalBuiltin ctx "getFactsByPrefix" [VString factType, VString prefix] =
  let matching = [ v
                 | ((ft, key), v) <- Map.toList (ctxFacts ctx)
                 , ft == factType
                 , prefix `T.isPrefixOf` key
                 ]
  in Right $ VList matching
evalBuiltin _ "recordGet" [VRecord rec, VString field'] =
  case Map.lookup field' rec of
    Just v -> Right $ VSome v
    Nothing -> Right VNone
evalBuiltin _ "isSome" [VNone] = Right $ VBool False
evalBuiltin _ "isSome" [VSome _] = Right $ VBool True
evalBuiltin _ "unwrapOr" [VNone, defaultVal] = Right defaultVal
evalBuiltin _ "unwrapOr" [VSome v, _] = Right v
evalBuiltin _ "requireSome" [VNone, VString errCode, VString msg] =
  Left $ NullValue msg errCode
evalBuiltin _ "requireSome" [VSome v, _, _] = Right v
evalBuiltin _ "toDec" [VInt scale, VInt value] =
  Right $ VDec (fromIntegral scale) value
evalBuiltin _ "toQty" [VString unit, VInt value] =
  Right $ VQty 6 value unit
evalBuiltin _ "convert" [VString _targetUnit, VString _srcUnit, VQty _s _v _u] =
  -- TODO: Implement proper unit conversion
  Right $ VQty 6 0 _targetUnit
evalBuiltin _ "sumQty" [VString unit, VList items] = do
  let sumItems [] acc = Right acc
      sumItems (VQty s v u : rest) acc
        | u == unit = sumItems rest (acc + v)
        | otherwise = Left $ AssertionFailed ("Unit mismatch in sumQty: expected " <> unit <> ", got " <> u) "UNIT_MISMATCH"
      sumItems _ _ = Left $ AssertionFailed "sumQty requires list of Qty" "TYPE_ERROR"
  total <- sumItems items 0
  Right $ VQty 6 total unit
evalBuiltin _ "sumDec" [VInt scale, VList items] = do
  let s = fromIntegral scale
  let sumItems [] acc = Right acc
      sumItems (VDec ds v : rest) acc
        | ds == s = sumItems rest (acc + v)
        | otherwise = Left $ AssertionFailed "Dec scale mismatch in sumDec" "TYPE_ERROR"
      sumItems _ _ = Left $ AssertionFailed "sumDec requires list of Dec" "TYPE_ERROR"
  total <- sumItems items 0
  Right $ VDec s total
evalBuiltin _ "map" [VList items, _lambda] =
  -- Placeholder: lambda evaluation needs more work
  Right $ VList items
evalBuiltin _ "filter" [VList items, _lambda] =
  -- Placeholder: lambda evaluation needs more work
  Right $ VList items
evalBuiltin _ "fold" [VList _items, initial, _lambda] =
  -- Placeholder: lambda evaluation needs more work
  Right initial
evalBuiltin _ "emitCompliance" [VString _clause, VString _status, VString _evidence] =
  Right $ VBool True  -- Side effect: emit compliance record
evalBuiltin _ name _ =
  Left $ FactNotFound "builtin" name
