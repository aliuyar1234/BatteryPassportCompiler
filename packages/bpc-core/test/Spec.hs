module Main (main) where

import Test.Tasty

import qualified BPC.Core.CanonicalJsonSpec
import qualified BPC.Core.DecimalSpec
import qualified BPC.Core.DomainSpec
import qualified BPC.Core.HashSpec
import qualified BPC.Core.QuantitySpec
import qualified BPC.Core.Rules.ParserSpec
import qualified BPC.Core.Rules.TypecheckSpec
import qualified BPC.Core.Rules.GraphSpec
import qualified BPC.Core.Rules.BuiltinsSpec
import qualified BPC.Core.Rules.EvalSpec
import qualified BPC.Core.CompileSpec
import qualified BPC.Core.ProofSpec
import qualified BPC.Core.ReceiptSpec
import qualified BPC.Core.QRSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "BPC.Core Tests"
  [ testGroup "Core Primitives"
      [ BPC.Core.CanonicalJsonSpec.tests
      , BPC.Core.HashSpec.tests
      , BPC.Core.DomainSpec.tests
      , BPC.Core.DecimalSpec.tests
      , BPC.Core.QuantitySpec.tests
      ]
  , testGroup "Rule Engine"
      [ BPC.Core.Rules.ParserSpec.tests
      , BPC.Core.Rules.TypecheckSpec.tests
      , BPC.Core.Rules.GraphSpec.tests
      , BPC.Core.Rules.BuiltinsSpec.tests
      , BPC.Core.Rules.EvalSpec.tests
      ]
  , testGroup "Compilation Pipeline"
      [ BPC.Core.CompileSpec.tests
      , BPC.Core.ProofSpec.tests
      , BPC.Core.ReceiptSpec.tests
      , BPC.Core.QRSpec.tests
      ]
  ]
