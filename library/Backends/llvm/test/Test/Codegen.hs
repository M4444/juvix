module Test.Codegen (top) where

import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Types as CodegenTypes
import qualified Juvix.Backends.LLVM.Codegen.Types.Sum as Sum
import Juvix.Library
import qualified LLVM.ExecutionEngine as Engine
import qualified LLVM.Module as Module
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

top :: TestTree
top = testGroup "LLVM Codegen tests" tests

tests :: [TestTree]
tests =
  [trivialLLVMCodegenTest]

trivialLLVMCodegenTest :: TestTree
trivialLLVMCodegenTest = testCase "Trivial LLVM codegen test" $ True @?= True
