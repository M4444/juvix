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
import qualified LLVM.Pretty as Pretty
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

llvmIntType :: Word32 -> PassTypes.TypeLLVM
llvmIntType size = PassTypes.PrimTy $ Primitive.PrimTy $ AST.IntegerType {typeBits = size}

llvmIntVal :: Integer -> PassTypes.TermLLVM
llvmIntVal = PassTypes.Prim . Primitive.LitInt

llvmAnnotatedTerm :: PassTypes.TypeLLVM -> PassTypes.TermLLVM -> PassTypes.Annotated PassTypes.TermLLVM
llvmAnnotatedTerm ty term = PassTypes.Ann {PassTypes.usage = Usage.SAny, PassTypes.annTy = ty, PassTypes.term = term}

top :: TestTree
top = testGroup "LLVM Codegen tests" tests

tests :: [TestTree]
tests =
  [trivialLLVMCodegenClosureTest, trivialLLVMCodegenAlgebraicTest]

trivialLLVMCodegenClosureTest :: TestTree
trivialLLVMCodegenClosureTest = testCase "Trivial LLVM codegen test with closure type" $ do
  let testVal = 42
      ty = llvmIntType 8
      term = llvmIntVal testVal
      compiled =
        fromRight (P.error "trivial test compilation failed") $
          Compilation.termLLVMToModule $ llvmAnnotatedTerm ty term
  output <- runIntModule compiled
  let casted :: Int8 = fromIntegral output
  casted @?= fromIntegral testVal

trivialLLVMCodegenAlgebraicTest :: TestTree
trivialLLVMCodegenAlgebraicTest = testCase "Trivial LLVM codegen test with algebraic types" $ do
  let test8aVal = 43
      test8bVal = 44
      test16Val = 45
      int8Ty = llvmIntType 8
      int16Ty = llvmIntType 16
      recordTypeName = "testRecordType"
      field8aName = "testFieldInt8a"
      field8bName = "testFieldInt8b"
      field16Name = "testFieldInt16"
      testRecordTy = (recordTypeName, [(field8aName, int8Ty), (field8bName, int8Ty), (field16Name, int16Ty)])
      internalTerm8a = llvmIntVal test8aVal
      annotatedTerm8a = llvmAnnotatedTerm int8Ty internalTerm8a
      internalTerm8b = llvmIntVal test8bVal
      annotatedTerm8b = llvmAnnotatedTerm int8Ty internalTerm8b
      internalTerm16 = llvmIntVal test16Val
      annotatedTerm16 = llvmAnnotatedTerm int16Ty internalTerm16
      testRecordTerm = PassTypes.RecordM (recordTypeName, [annotatedTerm8a, annotatedTerm8b, annotatedTerm16])
      annotatedRecordTerm = llvmAnnotatedTerm (PassTypes.RecordType recordTypeName) testRecordTerm
      testFieldTerm = PassTypes.FieldM (recordTypeName, field8bName, annotatedRecordTerm)
      internalAnnotatedTerm = llvmAnnotatedTerm int8Ty testFieldTerm
      term = PassTypes.ScopedRecordDeclM testRecordTy internalAnnotatedTerm
      annotatedTerm = llvmAnnotatedTerm int8Ty term
      compiled =
        fromRight (P.error "trivial test compilation failed") $
          Compilation.termLLVMToModule annotatedTerm
  putStrLn $ "LLVM AST: " <> Pretty.ppllvm compiled
  output <- runIntModule compiled
  let casted :: Int8 = fromIntegral output
  casted @?= fromIntegral test8bVal
