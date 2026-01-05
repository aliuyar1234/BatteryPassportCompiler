{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Types.Quantity - Physical quantities with units
--
-- Provides type-safe handling of physical quantities with units.
-- Unit checking happens at runtime to allow flexible data processing.
module BPC.Core.Types.Quantity
  ( -- * Quantity Type
    Qty (..)

    -- * Construction
  , mkQty

    -- * Conversion
  , convertQty

    -- * Arithmetic
  , addQty
  , subQty
  , mulQtyDec
  , divQtyDec

    -- * Aggregation
  , sumQty
  ) where

import BPC.Core.Error (UnitError (..))
import BPC.Core.Types.Decimal (Dec (..), addDec, divDec, mulDec, subDec)
import BPC.Core.Types.Units (conversionFactor, isValidUnit)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)

-- | A physical quantity with a unit.
--
-- Combines a decimal value with a unit symbol.
-- Unit checking is performed at runtime.
data Qty scale = Qty
  { qtyValue :: !(Dec scale)
    -- ^ The numeric value
  , qtyUnit :: !Text
    -- ^ The unit symbol
  }
  deriving stock (Eq, Show, Generic)

-- | Create a quantity, validating the unit.
mkQty :: Text -> Dec scale -> Either UnitError (Qty scale)
mkQty unit val
  | isValidUnit unit = Right $ Qty val unit
  | otherwise = Left $ UnknownUnit unit

-- | Convert a quantity to a different unit.
--
-- Returns an error if the units are incompatible.
convertQty :: KnownNat scale => Qty scale -> Text -> Either UnitError (Qty scale)
convertQty (Qty val fromUnit) toUnit = do
  factor <- maybe (Left $ IncompatibleUnits fromUnit toUnit) Right $
    conversionFactor fromUnit toUnit

  -- Apply conversion factor
  let scaledVal = unDec val
  let converted = Dec $ round (fromRational factor * fromInteger scaledVal)

  if isValidUnit toUnit
    then Right $ Qty converted toUnit
    else Left $ UnknownUnit toUnit

-- | Add two quantities of the same unit.
addQty :: Qty scale -> Qty scale -> Either UnitError (Qty scale)
addQty (Qty v1 u1) (Qty v2 u2)
  | u1 /= u2 = Left $ UnitMismatch u1 u2
  | otherwise = Right $ Qty (addDec v1 v2) u1

-- | Subtract two quantities of the same unit.
subQty :: Qty scale -> Qty scale -> Either UnitError (Qty scale)
subQty (Qty v1 u1) (Qty v2 u2)
  | u1 /= u2 = Left $ UnitMismatch u1 u2
  | otherwise = Right $ Qty (subDec v1 v2) u1

-- | Multiply a quantity by a dimensionless decimal.
mulQtyDec :: KnownNat scale => Qty scale -> Dec scale -> Qty scale
mulQtyDec (Qty val unit) factor = Qty (mulDec val factor) unit

-- | Divide a quantity by a dimensionless decimal.
divQtyDec :: KnownNat scale => Qty scale -> Dec scale -> Either UnitError (Qty scale)
divQtyDec (Qty val unit) divisor =
  case divDec val divisor of
    Nothing -> Left DivisionByZero
    Just result -> Right $ Qty result unit

-- | Sum a list of quantities, all must have the same unit.
sumQty :: [Qty scale] -> Either UnitError (Qty scale)
sumQty [] = Left $ UnknownUnit "empty list"
sumQty (first : rest) = foldl addNext (Right first) rest
  where
    addNext (Left e) _ = Left e
    addNext (Right acc) q = addQty acc q

-- JSON instances

instance KnownNat scale => ToJSON (Qty scale) where
  toJSON (Qty val unit) = object
    [ "value" .= val
    , "unit" .= unit
    ]

instance KnownNat scale => FromJSON (Qty scale) where
  parseJSON = withObject "Qty" $ \o -> do
    val <- o .: "value"
    unit <- o .: "unit"
    pure $ Qty val unit
