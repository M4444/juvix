module Test.Codegen (top) where

import qualified Juvix.Core.Categorial ()
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library (Bool (True), ($))
import qualified Juvix.Library.Usage as Usage
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Prelude as P

top :: TestTree
top = testGroup "Interpreted 'codegen' tests" tests

tests :: [TestTree]
tests =
  [ trivialInterpretedCodegenTest
  ]

trivialInterpretedCodegenTest :: TestTree
trivialInterpretedCodegenTest = testCase "Trivial interpreted 'codegen' test" $ do
  True @?= True
