{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Rules.Parser - Rule DSL Parser
--
-- Parses Rule DSL source code into untyped AST following SSOT 8.1 EBNF grammar.
module BPC.Core.Rules.Parser
  ( -- * Main Entry Point
    parseSource
  , parseSourceFile

    -- * Expression Parsers
  , pExpr
  , pFieldDecl
  , pLetExpr
  , pIfExpr
  , pAssertExpr
  , pFuncCall

    -- * Declaration Parsers
  , pDeclaration
  , pModule
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

import BPC.Core.Rules.AST
import qualified BPC.Core.Rules.Error as E
import BPC.Core.Rules.Lexer

-- | Parse DSL source code into a Module.
parseSource :: Text -> Either E.ParseError Module
parseSource input = case parse (sc *> pModule <* eof) "<input>" input of
  Left err -> Left $ E.ParseError
    { E.parseErrorLine = 1  -- TODO: Extract from error bundle
    , E.parseErrorColumn = 1
    , E.parseErrorMessage = T.pack $ errorBundlePretty err
    }
  Right m -> Right m

-- | Parse DSL source from a file path (pure, file read happens outside).
parseSourceFile :: FilePath -> Text -> Either E.ParseError Module
parseSourceFile path input = case parse (sc *> pModule <* eof) path input of
  Left err -> Left $ E.ParseError
    { E.parseErrorLine = 1
    , E.parseErrorColumn = 1
    , E.parseErrorMessage = T.pack $ errorBundlePretty err
    }
  Right m -> Right m

-- | Parse a complete module (list of declarations).
pModule :: Parser Module
pModule = Module <$> many pDeclaration

-- | Parse a top-level declaration.
pDeclaration :: Parser Declaration
pDeclaration = choice
  [ DeclField <$> pFieldDecl
  , pDeclExample
  , pDeclProperty
  ]

-- | Parse a field declaration.
pFieldDecl :: Parser FieldDecl
pFieldDecl = do
  keyword "field"
  path <- fieldPath
  _ <- symbol ":"
  ty <- pType
  _ <- symbol "="
  expr <- pExpr
  _ <- symbol ";"
  pure $ FieldDecl (FieldPath path) ty expr

-- | Parse an example test declaration.
pDeclExample :: Parser Declaration
pDeclExample = do
  keyword "example"
  name <- litString
  _ <- symbol "{"
  assertions <- many pAssertExpr
  _ <- symbol "}"
  pure $ DeclExample name assertions

-- | Parse a property test declaration.
pDeclProperty :: Parser Declaration
pDeclProperty = do
  keyword "property"
  name <- litString
  _ <- symbol "{"
  expr <- pExpr
  _ <- symbol "}"
  pure $ DeclProperty name expr

-- | Parse a type annotation.
pType :: Parser Ty
pType = choice
  [ TBool <$ keyword "Bool"
  , TInt <$ keyword "Int"
  , pDecType
  , pQtyType
  , TString <$ keyword "String"
  , TDate <$ keyword "Date"
  , pOptionalType
  , pListType
  ]

-- | Parse Dec(scale) type.
pDecType :: Parser Ty
pDecType = do
  _ <- symbol "Dec"
  _ <- symbol "("
  scale <- fromIntegral <$> litInt
  _ <- symbol ")"
  pure $ TDec scale

-- | Parse Qty(unit) type.
pQtyType :: Parser Ty
pQtyType = do
  _ <- symbol "Qty"
  _ <- symbol "("
  unit <- litString
  _ <- symbol ")"
  pure $ TQty unit

-- | Parse Optional(T) type.
pOptionalType :: Parser Ty
pOptionalType = do
  _ <- symbol "Optional"
  _ <- symbol "("
  inner <- pType
  _ <- symbol ")"
  pure $ TOptional inner

-- | Parse List(T) type.
pListType :: Parser Ty
pListType = do
  _ <- symbol "List"
  _ <- symbol "("
  inner <- pType
  _ <- symbol ")"
  pure $ TList inner

-- | Parse an expression.
pExpr :: Parser UntypedExpr
pExpr = pOrExpr

-- | Parse OR expressions (lowest precedence).
pOrExpr :: Parser UntypedExpr
pOrExpr = do
  left <- pAndExpr
  rest <- many (symbol "||" *> pAndExpr)
  pure $ foldl (UBinOp "||") left rest

-- | Parse AND expressions.
pAndExpr :: Parser UntypedExpr
pAndExpr = do
  left <- pCmpExpr
  rest <- many (symbol "&&" *> pCmpExpr)
  pure $ foldl (UBinOp "&&") left rest

-- | Parse comparison expressions.
pCmpExpr :: Parser UntypedExpr
pCmpExpr = do
  left <- pAddExpr
  rest <- optional $ do
    op <- choice
      [ "==" <$ symbol "=="
      , "!=" <$ symbol "!="
      , "<=" <$ symbol "<="
      , ">=" <$ symbol ">="
      , "<" <$ symbol "<"
      , ">" <$ symbol ">"
      ]
    right <- pAddExpr
    pure (op, right)
  case rest of
    Nothing -> pure left
    Just (op, right) -> pure $ UBinOp op left right

-- | Parse additive expressions.
pAddExpr :: Parser UntypedExpr
pAddExpr = do
  left <- pMulExpr
  rest <- many $ do
    op <- choice ["+" <$ symbol "+", "-" <$ symbol "-"]
    right <- pMulExpr
    pure (op, right)
  pure $ foldl (\l (op, r) -> UBinOp op l r) left rest

-- | Parse multiplicative expressions.
pMulExpr :: Parser UntypedExpr
pMulExpr = do
  left <- pUnaryExpr
  rest <- many $ do
    op <- choice ["*" <$ symbol "*", "/" <$ symbol "/"]
    right <- pUnaryExpr
    pure (op, right)
  pure $ foldl (\l (op, r) -> UBinOp op l r) left rest

-- | Parse unary expressions.
pUnaryExpr :: Parser UntypedExpr
pUnaryExpr = choice
  [ UUnaryOp "!" <$> (symbol "!" *> pUnaryExpr)
  , UUnaryOp "-" <$> (symbol "-" *> pUnaryExpr)
  , pPrimaryExpr
  ]

-- | Parse primary expressions (highest precedence).
pPrimaryExpr :: Parser UntypedExpr
pPrimaryExpr = choice
  [ pLetExpr
  , pIfExpr
  , pAssertExpr
  , pLiteral
  , pFuncCallOrVar
  , pFieldRef
  , pParenExpr
  ]

-- | Parse a let expression.
pLetExpr :: Parser UntypedExpr
pLetExpr = do
  keyword "let"
  name <- identifier
  _ <- symbol "="
  value <- pExpr
  _ <- symbol ";"
  body <- pExpr
  pure $ ULet (Identifier name) value body

-- | Parse an if/then/else expression.
pIfExpr :: Parser UntypedExpr
pIfExpr = do
  keyword "if"
  cond <- pExpr
  keyword "then"
  thenBranch <- pExpr
  keyword "else"
  elseBranch <- pExpr
  pure $ UIf cond thenBranch elseBranch

-- | Parse an assert expression.
pAssertExpr :: Parser UntypedExpr
pAssertExpr = do
  keyword "assert"
  _ <- symbol "("
  cond <- pExpr
  _ <- symbol ","
  errorCode <- litString
  _ <- symbol ","
  message <- litString
  _ <- symbol ")"
  pure $ UAssert cond errorCode message

-- | Parse a function call or variable reference.
pFuncCallOrVar :: Parser UntypedExpr
pFuncCallOrVar = do
  name <- identifier
  args <- optional $ do
    _ <- symbol "("
    as <- pExpr `sepBy` symbol ","
    _ <- symbol ")"
    pure as
  case args of
    Nothing -> pure $ UVar (Identifier name)
    Just as -> pure $ UFuncCall (Identifier name) as

-- | Parse a function call.
pFuncCall :: Parser UntypedExpr
pFuncCall = do
  name <- identifier
  _ <- symbol "("
  args <- pExpr `sepBy` symbol ","
  _ <- symbol ")"
  pure $ UFuncCall (Identifier name) args

-- | Parse a field reference like @battery.capacity_kwh.
pFieldRef :: Parser UntypedExpr
pFieldRef = do
  _ <- char '@'
  path <- fieldPath
  pure $ UFieldRef (FieldPath path)

-- | Parse a parenthesized expression.
pParenExpr :: Parser UntypedExpr
pParenExpr = do
  _ <- symbol "("
  e <- pExpr
  _ <- symbol ")"
  pure e

-- | Parse a literal value.
pLiteral :: Parser UntypedExpr
pLiteral = choice
  [ ULitBool <$> litBool
  , try (uncurry ULitDec <$> litDec)
  , ULitInt <$> litInt
  , ULitString <$> litString
  ]
