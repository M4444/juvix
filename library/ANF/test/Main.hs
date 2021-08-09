module Main where

import Juvix.Library (IO)
import qualified Test.Tasty as T

effectHandlerTests :: T.TestTree
effectHandlerTests =
  T.testGroup
    "CPS translation"
    []

anfTransTests :: T.TestTree
anfTransTests =
  T.testGroup
    "ANF translation"
    []

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ effectHandlerTests,
      anfTransTests
    ]

main :: IO ()
main = T.defaultMain allCheckedTests
