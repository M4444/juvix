module Test.Parameterization (top) where

import Juvix.Backends.Interpreter ()
import Juvix.Core.Parameterisation (PrimType (PrimType), hasType)
import Juvix.Library (Bool (True), ($))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

top :: TestTree
top = testGroup "Interpreter Parameterization tests" tests

tests :: [TestTree]
tests =
  [ trivialInterpreterParameterizationTest
  ]

trivialInterpreterParameterizationTest :: TestTree
trivialInterpreterParameterizationTest = testCase "trivial interpreter parameterization test" $ do
  True @?= True
