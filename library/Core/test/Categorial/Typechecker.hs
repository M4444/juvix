module Categorial.Typechecker (top) where

import qualified Categorial.Utils ()
import qualified Juvix.Core.Categorial ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Typechecker ()
import Prelude

top :: TestTree
top = testGroup "Categorial type tests" tests

tests :: [TestTree]
tests = [categorialTypecheckTest]

categorialTypecheckTest :: TestTree
categorialTypecheckTest =
  testCase "Categorial type checking" $
    True @?= True
