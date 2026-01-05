{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | BPC.Core.Types.Decimal - Fixed-precision decimal arithmetic
--
-- Provides exact decimal arithmetic with compile-time scale tracking.
-- Used for financial and measurement values where floating point
-- imprecision is unacceptable.
module BPC.Core.Types.Decimal
  ( -- * Decimal Type
    Dec (..)

    -- * Construction
  , toDec
  , fromInteger'
  , zero

    -- * Conversion
  , toRational'
  , toScientific

    -- * Arithmetic (same scale)
  , addDec
  , subDec

    -- * Scale-changing operations
  , mulDec
  , divDec
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)

-- | Fixed-precision decimal with scale encoded in the type.
--
-- @Dec 6@ represents a decimal with 6 decimal places.
-- Internally stored as an Integer scaled by 10^scale.
--
-- >>> let x = toDec @6 1234567  -- Represents 1.234567
-- >>> let y = toDec @6 1000000  -- Represents 1.000000
-- >>> addDec x y
-- Dec 2234567  -- Represents 2.234567
newtype Dec (scale :: Nat) = Dec { unDec :: Integer }
  deriving stock (Eq, Ord, Show, Generic)

-- | Create a Dec from an unscaled Integer value.
--
-- The integer is interpreted as already being scaled by 10^scale.
--
-- >>> toDec @6 1500000  -- Represents 1.5
-- Dec 1500000
toDec :: forall scale. KnownNat scale => Integer -> Dec scale
toDec = Dec

-- | Create a Dec from an Integer (whole number).
--
-- The integer is scaled up by 10^scale.
--
-- >>> fromInteger' @6 5  -- Represents 5.000000
-- Dec 5000000
fromInteger' :: forall scale. KnownNat scale => Integer -> Dec scale
fromInteger' n = Dec (n * (10 ^ natVal (Proxy @scale)))

-- | Zero value.
zero :: Dec scale
zero = Dec 0

-- | Convert to Rational.
toRational' :: forall scale. KnownNat scale => Dec scale -> Rational
toRational' (Dec n) = fromInteger n / fromInteger (10 ^ natVal (Proxy @scale))

-- | Convert to Scientific.
toScientific :: forall scale. KnownNat scale => Dec scale -> Scientific
toScientific (Dec n) =
  Scientific.scientific n (negate $ fromIntegral $ natVal (Proxy @scale))

-- | Add two decimals of the same scale.
addDec :: Dec scale -> Dec scale -> Dec scale
addDec (Dec a) (Dec b) = Dec (a + b)

-- | Subtract two decimals of the same scale.
subDec :: Dec scale -> Dec scale -> Dec scale
subDec (Dec a) (Dec b) = Dec (a - b)

-- | Multiply two decimals.
--
-- Result scale is the sum of input scales, but we truncate to the first scale.
-- This is a simplified implementation for MVP.
mulDec :: forall scale. KnownNat scale => Dec scale -> Dec scale -> Dec scale
mulDec (Dec a) (Dec b) =
  let scaleFactor = 10 ^ natVal (Proxy @scale)
   in Dec ((a * b) `div` scaleFactor)

-- | Divide two decimals.
--
-- Uses integer division after scaling. May lose precision.
divDec :: forall scale. KnownNat scale => Dec scale -> Dec scale -> Maybe (Dec scale)
divDec _ (Dec 0) = Nothing
divDec (Dec a) (Dec b) =
  let scaleFactor = 10 ^ natVal (Proxy @scale)
   in Just $ Dec ((a * scaleFactor) `div` b)

-- JSON instances

instance KnownNat scale => ToJSON (Dec scale) where
  toJSON = Aeson.Number . toScientific

instance KnownNat scale => FromJSON (Dec scale) where
  parseJSON = Aeson.withScientific "Dec" $ \s -> do
    let scale = natVal (Proxy @scale)
    let scaled = Scientific.coefficient s * (10 ^ (scale + fromIntegral (Scientific.base10Exponent s)))
    pure $ Dec scaled
