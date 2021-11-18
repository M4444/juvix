module Main where

import Conv (coreConversions)
import Erasure (erasureTests)
import qualified HR.Pretty
import qualified IR.Weak as Weak
import Juvix.Library (IO)
import Juvix.Library.Fetch (loadStdLibs)
import qualified Test.Tasty as T
import Typechecker (coreCheckerEval)
import Unify (coreUnifier)
import qualified Utility

coreTests :: T.TestTree
coreTests =
  T.testGroup
    "Core tests"
    [ coreCheckerEval,
      coreConversions,
      coreUnifier
    ]

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ coreTests,
      erasureTests,
      Weak.top,
      HR.Pretty.top,
      Utility.top
    ]

main :: IO ()
main = do
  loadStdLibs
  T.defaultMain allCheckedTests
