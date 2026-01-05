{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Types.Units - Physical unit definitions and conversions
--
-- Defines all supported units for BPC quantities and their
-- conversion relationships.
module BPC.Core.Types.Units
  ( -- * Unit Definition
    UnitDef (..)

    -- * Supported Units
  , supportedUnits
  , isValidUnit

    -- * Conversions
  , conversionFactor
  , canConvert
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Definition of a physical unit.
data UnitDef = UnitDef
  { unitSymbol :: !Text
    -- ^ The unit symbol (e.g., "kg", "kWh")
  , unitName :: !Text
    -- ^ Human-readable name
  , unitBaseUnit :: !Text
    -- ^ The base unit for conversions
  , unitToBase :: !Rational
    -- ^ Multiplier to convert to base unit
  }
  deriving stock (Eq, Show)

-- | All supported units in BPC.
--
-- Units are organized by category:
--   * Mass: kg, g
--   * Energy: kWh, Wh
--   * Carbon: kgCO2e, gCO2e
--   * Intensity: gCO2e_per_kWh
--   * Dimensionless: pct (percent), each
supportedUnits :: Map Text UnitDef
supportedUnits = Map.fromList
  [ -- Mass
    ("kg", UnitDef "kg" "kilogram" "g" 1000)
  , ("g", UnitDef "g" "gram" "g" 1)

    -- Energy
  , ("kWh", UnitDef "kWh" "kilowatt-hour" "Wh" 1000)
  , ("Wh", UnitDef "Wh" "watt-hour" "Wh" 1)

    -- Carbon emissions
  , ("kgCO2e", UnitDef "kgCO2e" "kilogram CO2 equivalent" "gCO2e" 1000)
  , ("gCO2e", UnitDef "gCO2e" "gram CO2 equivalent" "gCO2e" 1)

    -- Carbon intensity
  , ("gCO2e_per_kWh", UnitDef "gCO2e_per_kWh" "grams CO2e per kWh" "gCO2e_per_kWh" 1)

    -- Dimensionless
  , ("pct", UnitDef "pct" "percent" "pct" 1)
  , ("each", UnitDef "each" "count" "each" 1)
  ]

-- | Check if a unit symbol is valid.
isValidUnit :: Text -> Bool
isValidUnit u = Map.member u supportedUnits

-- | Get conversion factor between two units.
--
-- Returns 'Just factor' where (value in u1) * factor = (value in u2).
-- Returns 'Nothing' if units are incompatible.
conversionFactor :: Text -> Text -> Maybe Rational
conversionFactor fromUnit toUnit = do
  fromDef <- Map.lookup fromUnit supportedUnits
  toDef <- Map.lookup toUnit supportedUnits

  -- Units must share the same base unit to be convertible
  if unitBaseUnit fromDef /= unitBaseUnit toDef
    then Nothing
    else Just $ unitToBase fromDef / unitToBase toDef

-- | Check if two units can be converted to each other.
canConvert :: Text -> Text -> Bool
canConvert u1 u2 = case conversionFactor u1 u2 of
  Just _ -> True
  Nothing -> False
