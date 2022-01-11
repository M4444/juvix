module Test.Codegen (top) where

import qualified GHC.Ptr as Ptr
import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Types as CodegenTypes
import qualified Juvix.Backends.LLVM.Codegen.Types.Sum as Sum
import qualified Juvix.Backends.LLVM.Compilation as Compilation
import qualified Juvix.Backends.LLVM.Pass.Types as PassTypes
import qualified Juvix.Backends.LLVM.Primitive as Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import qualified LLVM.AST as AST
import qualified LLVM.ExecutionEngine as Engine
import qualified LLVM.Internal.Context as LLVMContext
import qualified LLVM.Module as LLVMModule
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Prelude as P

foreign import ccall "dynamic" intFun :: FunPtr (IO Int) -> IO Int

runIntFun f = intFun (Ptr.castFunPtr f :: FunPtr (IO Int))

runIntModule :: AST.Module -> IO Int
runIntModule mod =
  LLVMContext.withContext $ \context ->
    LLVMModule.withModuleFromAST context mod $ \m ->
      Engine.withMCJIT context Nothing Nothing Nothing Nothing $ \executionEngine ->
        Engine.withModuleInEngine executionEngine m $ \ee -> do
          main <- Engine.getFunction ee (AST.Name "main")
          case main of
            Just f -> runIntFun f
            Nothing -> P.error "no main function found"

top :: TestTree
top = testGroup "LLVM Codegen tests" tests

tests :: [TestTree]
tests =
  [trivialLLVMCodegenTest]

trivialLLVMCodegenTest :: TestTree
trivialLLVMCodegenTest = testCase "Trivial LLVM codegen test" $ do
  let testVal = 42
      ty = PassTypes.PrimTy $ Primitive.PrimTy $ AST.IntegerType {typeBits = 8}
      term = PassTypes.Prim (Primitive.LitInt testVal)
      compiled =
        fromRight (P.error "trivial test compilation failed") $
          Compilation.termLLVMToModule PassTypes.Ann {usage = Usage.SAny, annTy = ty, term = term}
  output <- runIntModule compiled
  output @?= fromIntegral testVal
