{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Rules.Builtins - Built-in Functions for Rule DSL
--
-- Defines all 16 built-in functions from SSOT 8.2 with their
-- type signatures and arity information.
module BPC.Core.Rules.Builtins
  ( -- * Built-in Enum
    Builtin (..)
  , allBuiltins

    -- * Type Signatures
  , BuiltinSig (..)
  , builtinSignature

    -- * Lookup
  , lookupBuiltin
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import BPC.Core.Rules.AST (Ty (..))

-- | Enumeration of all built-in functions.
data Builtin
  = BGetFact           -- ^ getFact(factType, factKey) -> Opt(Record)
  | BGetFactsByPrefix  -- ^ getFactsByPrefix(factType, prefix) -> List(Record)
  | BField             -- ^ field(path) -> a
  | BRecordGet         -- ^ recordGet(record, field) -> Opt(a)
  | BIsSome            -- ^ isSome(opt) -> Bool
  | BUnwrapOr          -- ^ unwrapOr(opt, default) -> a
  | BRequireSome       -- ^ requireSome(opt, errCode, msg) -> a
  | BToDec             -- ^ toDec(scale, value) -> Dec(scale)
  | BToQty             -- ^ toQty(unit, value) -> Qty(unit)
  | BConvert           -- ^ convert(targetUnit, sourceUnit, qty) -> Qty(targetUnit)
  | BSumQty            -- ^ sumQty(unit, list) -> Qty(unit)
  | BSumDec            -- ^ sumDec(scale, list) -> Dec(scale)
  | BMap               -- ^ map(list, lambda) -> List(b)
  | BFilter            -- ^ filter(list, predicate) -> List(a)
  | BFold              -- ^ fold(list, initial, lambda) -> b
  | BEmitCompliance    -- ^ emitCompliance(clause, status, evidence) -> Bool
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | List of all built-in functions.
allBuiltins :: [Builtin]
allBuiltins = [minBound .. maxBound]

-- | Type signature for a built-in function.
data BuiltinSig = BuiltinSig
  { sigName :: !Text          -- ^ Function name
  , sigArity :: !Int          -- ^ Number of arguments
  , sigArgTypes :: ![Ty]      -- ^ Argument types (simplified)
  , sigReturnType :: !Ty      -- ^ Return type (simplified)
  , sigDescription :: !Text   -- ^ Brief description
  }
  deriving stock (Eq, Show)

-- | Get the type signature for a built-in function.
builtinSignature :: Builtin -> BuiltinSig
builtinSignature BGetFact = BuiltinSig
  { sigName = "getFact"
  , sigArity = 2
  , sigArgTypes = [TString, TString]
  , sigReturnType = TOptional (TRecord [])
  , sigDescription = "Lookup a fact by type and key"
  }
builtinSignature BGetFactsByPrefix = BuiltinSig
  { sigName = "getFactsByPrefix"
  , sigArity = 2
  , sigArgTypes = [TString, TString]
  , sigReturnType = TList (TRecord [])
  , sigDescription = "Get all facts of type matching key prefix"
  }
builtinSignature BField = BuiltinSig
  { sigName = "field"
  , sigArity = 1
  , sigArgTypes = [TString]
  , sigReturnType = TString  -- Polymorphic, depends on field type
  , sigDescription = "Reference another field by path"
  }
builtinSignature BRecordGet = BuiltinSig
  { sigName = "recordGet"
  , sigArity = 2
  , sigArgTypes = [TRecord [], TString]
  , sigReturnType = TOptional TString  -- Polymorphic
  , sigDescription = "Extract a field from a record"
  }
builtinSignature BIsSome = BuiltinSig
  { sigName = "isSome"
  , sigArity = 1
  , sigArgTypes = [TOptional TString]  -- Polymorphic
  , sigReturnType = TBool
  , sigDescription = "Check if optional has a value"
  }
builtinSignature BUnwrapOr = BuiltinSig
  { sigName = "unwrapOr"
  , sigArity = 2
  , sigArgTypes = [TOptional TString, TString]  -- Polymorphic
  , sigReturnType = TString  -- Same as inner type
  , sigDescription = "Unwrap optional or use default"
  }
builtinSignature BRequireSome = BuiltinSig
  { sigName = "requireSome"
  , sigArity = 3
  , sigArgTypes = [TOptional TString, TString, TString]
  , sigReturnType = TString  -- Same as inner type
  , sigDescription = "Unwrap optional or throw error"
  }
builtinSignature BToDec = BuiltinSig
  { sigName = "toDec"
  , sigArity = 2
  , sigArgTypes = [TInt, TInt]
  , sigReturnType = TDec 6  -- Scale determined by first arg
  , sigDescription = "Convert to decimal with given scale"
  }
builtinSignature BToQty = BuiltinSig
  { sigName = "toQty"
  , sigArity = 2
  , sigArgTypes = [TString, TInt]
  , sigReturnType = TQty ""  -- Unit determined by first arg
  , sigDescription = "Create quantity with unit"
  }
builtinSignature BConvert = BuiltinSig
  { sigName = "convert"
  , sigArity = 3
  , sigArgTypes = [TString, TString, TQty ""]
  , sigReturnType = TQty ""  -- Target unit
  , sigDescription = "Convert quantity to different unit"
  }
builtinSignature BSumQty = BuiltinSig
  { sigName = "sumQty"
  , sigArity = 2
  , sigArgTypes = [TString, TList (TQty "")]
  , sigReturnType = TQty ""
  , sigDescription = "Sum list of quantities"
  }
builtinSignature BSumDec = BuiltinSig
  { sigName = "sumDec"
  , sigArity = 2
  , sigArgTypes = [TInt, TList (TDec 6)]
  , sigReturnType = TDec 6
  , sigDescription = "Sum list of decimals"
  }
builtinSignature BMap = BuiltinSig
  { sigName = "map"
  , sigArity = 2
  , sigArgTypes = [TList TString, TString]  -- List(a), (a -> b)
  , sigReturnType = TList TString  -- List(b)
  , sigDescription = "Apply function to each element"
  }
builtinSignature BFilter = BuiltinSig
  { sigName = "filter"
  , sigArity = 2
  , sigArgTypes = [TList TString, TString]  -- List(a), (a -> Bool)
  , sigReturnType = TList TString  -- List(a)
  , sigDescription = "Keep elements matching predicate"
  }
builtinSignature BFold = BuiltinSig
  { sigName = "fold"
  , sigArity = 3
  , sigArgTypes = [TList TString, TString, TString]  -- List(a), b, (b -> a -> b)
  , sigReturnType = TString  -- b
  , sigDescription = "Fold list with accumulator"
  }
builtinSignature BEmitCompliance = BuiltinSig
  { sigName = "emitCompliance"
  , sigArity = 3
  , sigArgTypes = [TString, TString, TString]  -- clause, status, evidence
  , sigReturnType = TBool
  , sigDescription = "Emit compliance record (side effect)"
  }

-- | Lookup map from name to Builtin.
builtinLookupMap :: Map Text Builtin
builtinLookupMap = Map.fromList
  [ ("getFact", BGetFact)
  , ("getFactsByPrefix", BGetFactsByPrefix)
  , ("field", BField)
  , ("recordGet", BRecordGet)
  , ("isSome", BIsSome)
  , ("unwrapOr", BUnwrapOr)
  , ("requireSome", BRequireSome)
  , ("toDec", BToDec)
  , ("toQty", BToQty)
  , ("convert", BConvert)
  , ("sumQty", BSumQty)
  , ("sumDec", BSumDec)
  , ("map", BMap)
  , ("filter", BFilter)
  , ("fold", BFold)
  , ("emitCompliance", BEmitCompliance)
  ]

-- | Look up a built-in function by name.
lookupBuiltin :: Text -> Maybe Builtin
lookupBuiltin = flip Map.lookup builtinLookupMap
