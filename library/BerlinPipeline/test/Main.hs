module Main where

import qualified Feedback
import Juvix.Library (IO)
import Juvix.Library.Fetch (loadStdLibs)
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [Feedback.top]

main :: IO ()
main = do
  loadStdLibs
  T.defaultMain allCheckedTests
