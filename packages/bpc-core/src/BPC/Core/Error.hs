-- | BPC.Core.Error - Error types for core primitives
--
-- All error types used by bpc-core modules.
module BPC.Core.Error
  ( -- * Canonical JSON Errors
    CanonicalError (..)

    -- * Unit Arithmetic Errors
  , UnitError (..)
  ) where

import Data.Text (Text)

-- | Errors that can occur during canonical JSON encoding/decoding.
data CanonicalError
  = -- | Floats and exponential notation are not allowed in canonical JSON
    CanonicalNumberNotAllowed !Text
  | -- | Failed to parse JSON
    CanonicalParseError !Text
  | -- | Object key ordering is invalid
    CanonicalKeyOrderError !Text
  deriving stock (Eq, Show)

-- | Errors that can occur during unit arithmetic operations.
data UnitError
  = -- | Cannot perform operation on mismatched units
    UnitMismatch !Text !Text
  | -- | Unknown unit identifier
    UnknownUnit !Text
  | -- | Cannot convert between incompatible units
    IncompatibleUnits !Text !Text
  | -- | Division by zero
    DivisionByZero
  deriving stock (Eq, Show)
