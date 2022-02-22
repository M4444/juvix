module Main where

import Juvix.Library
import Juvix.Library.Fetch (loadStdLibs)
import qualified Test.Codegen as Codegen
import qualified Test.Golden as Golden
import qualified Test.Parameterization as Parameterization
import qualified Test.Tasty as T

main :: IO ()
main = do
  loadStdLibs
  T.defaultMain $
    T.testGroup "LLVM tests" [Golden.top, Parameterization.top, Codegen.top]
