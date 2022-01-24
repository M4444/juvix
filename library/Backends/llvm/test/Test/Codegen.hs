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
import qualified LLVM.AST.Type as ASTTypes
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

llvmPrimIntType :: Word32 -> AST.Type
llvmPrimIntType size = AST.IntegerType {typeBits = size}

llvmIntType :: Word32 -> PassTypes.TypeLLVM
llvmIntType size = PassTypes.PrimTy $ Primitive.PrimTy $ AST.IntegerType {typeBits = size}

llvmStringType :: PassTypes.TypeLLVM
llvmStringType = PassTypes.PrimTy $ Primitive.PrimTy $ CodegenTypes.pointerOf $ AST.IntegerType {typeBits = 8}

llvmIntVal :: Integer -> PassTypes.TermLLVM
llvmIntVal = PassTypes.Prim . Primitive.LitInt

llvmStringVal :: Text -> PassTypes.TermLLVM
llvmStringVal = PassTypes.Prim . Primitive.LitString

llvmAnnotatedTerm :: PassTypes.TypeLLVM -> PassTypes.TermLLVM -> PassTypes.Annotated PassTypes.TermLLVM
llvmAnnotatedTerm ty term = PassTypes.Ann {PassTypes.usage = Usage.SAny, PassTypes.annTy = ty, PassTypes.term = term}

llvmFunctionType :: PassTypes.TypeLLVM -> PassTypes.TypeLLVM -> PassTypes.TypeLLVM
llvmFunctionType = PassTypes.Pi Usage.SAny

llvmConstantFunction ::
  PassTypes.TypeLLVM ->
  PassTypes.TypeLLVM ->
  Primitive.RawPrimVal ->
  PassTypes.Annotated PassTypes.TermLLVM
llvmConstantFunction domain codomain val =
  llvmAnnotatedTerm (llvmFunctionType domain codomain) $
    PassTypes.LamM ["0"] $ llvmAnnotatedTerm codomain (PassTypes.Prim val)

llvmCurry ::
  -- | Input: a type 'a'
  PassTypes.TypeLLVM ->
  -- | Input: a type 'b'
  PassTypes.TypeLLVM ->
  -- | Input: a type 'c'
  PassTypes.TypeLLVM ->
  -- | Input : A function of type ('a', 'b') -> 'c'
  PassTypes.Annotated PassTypes.TermLLVM ->
  -- | A term of type 'a'
  PassTypes.Annotated PassTypes.TermLLVM ->
  -- | Output: A function of type 'b' -> 'c'
  PassTypes.Annotated PassTypes.TermLLVM
llvmCurry a b c x f =
  llvmAnnotatedTerm
    (llvmFunctionType b c)
    ( PassTypes.LamM
        ["0"]
        ( llvmAnnotatedTerm
            c
            ( PassTypes.AppM
                f
                [llvmAnnotatedTerm b (PassTypes.Var "0"), x]
            )
        )
    )

llvmIntCurry ::
  Word32 ->
  Integer ->
  Primitive.RawPrimVal ->
  PassTypes.Annotated PassTypes.TermLLVM
llvmIntCurry b i f = llvmCurry intTy intTy intTy iAnn fAnn
  where
    iType = llvmPrimIntType b
    intTy = llvmIntType b
    iAnn = llvmAnnotatedTerm intTy $ llvmIntVal i
    fAnn = llvmAnnotatedTerm (PassTypes.PrimTy $ Primitive.PrimTy fType) $ PassTypes.Prim f
    fType = AST.FunctionType iType [iType, iType] False

llvmAddFunction :: Word32 -> Integer -> PassTypes.Annotated PassTypes.TermLLVM
llvmAddFunction b i = llvmIntCurry b i Primitive.Add

annotatedBinOp ::
  ASTTypes.Type ->
  Primitive.RawPrimVal ->
  PassTypes.Annotated PassTypes.TermLLVM
annotatedBinOp ty op =
  llvmAnnotatedTerm (PassTypes.PrimTy $ Primitive.PrimTy ty) $
    PassTypes.Prim op

annotatedBinOpApp ::
  PassTypes.TypeLLVM ->
  PassTypes.Annotated PassTypes.TermLLVM ->
  PassTypes.Annotated PassTypes.TermLLVM ->
  PassTypes.Annotated PassTypes.TermLLVM ->
  PassTypes.Annotated PassTypes.TermLLVM
annotatedBinOpApp ty op term term' = llvmAnnotatedTerm ty (PassTypes.AppM op [term, term'])

top :: TestTree
top = testGroup "LLVM Codegen tests" tests

tests :: [TestTree]
tests =
  [ trivialLLVMCodegenClosureTest,
    trivialLLVMCodegenRecordTest,
    llvmRecordNameShadowingTest,
    trivialLLVMCodegenSumTest
  ]

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

trivialLLVMCodegenRecordTest :: TestTree
trivialLLVMCodegenRecordTest = testCase "Trivial LLVM codegen test with record types" $ do
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
        fromRight (P.error "LLVM record types test compilation failed") $
          Compilation.termLLVMToModule annotatedTerm
  output <- runIntModule compiled
  let casted :: Int8 = fromIntegral output
  casted @?= fromIntegral test8bVal

llvmRecordNameShadowingTest :: TestTree
llvmRecordNameShadowingTest = testCase "LLVM codegen test with record name shadowing" $ do
  let shadowedName = "shadowedRec"
      shadowedFieldName = "shadowedField"
      shadowingFieldName = "shadowingField"
      intTy = llvmIntType 32
      addOp = annotatedBinOp ASTTypes.i32 Primitive.Add
      mulOp = annotatedBinOp ASTTypes.i32 Primitive.Mul
      shadowingVal = 1
      shadowedValLeft = 2
      shadowedValRight = 4
      shadowedTy = (shadowedName, [(shadowedFieldName, intTy)])
      shadowingTy = (shadowedName, [(shadowingFieldName, intTy)])
      shadowingInt = llvmIntVal shadowingVal
      shadowedIntLeft = llvmIntVal shadowedValLeft
      shadowedIntRight = llvmIntVal shadowedValRight
      shadowingAnnotated = llvmAnnotatedTerm intTy shadowingInt
      shadowedAnnotatedLeft = llvmAnnotatedTerm intTy shadowedIntLeft
      shadowedAnnotatedRight = llvmAnnotatedTerm intTy shadowedIntRight
      shadowingRecord = PassTypes.RecordM (shadowedName, [shadowingAnnotated])
      annotatedShadowingRecord =
        llvmAnnotatedTerm (PassTypes.RecordType shadowedName) shadowingRecord
      shadowingField =
        PassTypes.FieldM (shadowedName, shadowingFieldName, annotatedShadowingRecord)
      annotatedShadowingField = llvmAnnotatedTerm intTy shadowingField
      shadowingTerm = PassTypes.ScopedRecordDeclM shadowingTy annotatedShadowingField
      annotatedShadowingTerm = llvmAnnotatedTerm intTy shadowingTerm
      shadowedRecordLeft = PassTypes.RecordM (shadowedName, [shadowedAnnotatedLeft])
      shadowedRecordRight = PassTypes.RecordM (shadowedName, [shadowedAnnotatedRight])
      annotatedShadowedRecordLeft =
        llvmAnnotatedTerm (PassTypes.RecordType shadowedName) shadowedRecordLeft
      annotatedShadowedRecordRight =
        llvmAnnotatedTerm (PassTypes.RecordType shadowedName) shadowedRecordRight
      shadowedFieldLeft =
        PassTypes.FieldM (shadowedName, shadowedFieldName, annotatedShadowedRecordLeft)
      shadowedFieldRight =
        PassTypes.FieldM (shadowedName, shadowedFieldName, annotatedShadowedRecordRight)
      annotatedShadowedFieldLeft = llvmAnnotatedTerm intTy shadowedFieldLeft
      annotatedShadowedFieldRight = llvmAnnotatedTerm intTy shadowedFieldRight
      mulTerm = annotatedBinOpApp intTy mulOp annotatedShadowingTerm annotatedShadowedFieldRight
      addTerm = annotatedBinOpApp intTy addOp annotatedShadowedFieldLeft mulTerm
      shadowedTerm = PassTypes.ScopedRecordDeclM shadowedTy addTerm
      annotatedShadowedTerm = llvmAnnotatedTerm intTy shadowedTerm
      compiled =
        fromRight (P.error "name shadowing test compilation failed") $
          Compilation.termLLVMToModule annotatedShadowedTerm
  output <- runIntModule compiled
  let casted :: Int32 = fromIntegral output
  casted @?= fromIntegral (shadowedValLeft + (shadowingVal * shadowedValRight))

trivialLLVMCodegenSumTest :: TestTree
trivialLLVMCodegenSumTest = testCase "Trivial LLVM codegen test with sum types" $ do
  let sumTypeName = "testSumType"
      intVariantName = "testVariantInt"
      stringVariantName = "testVariantString"
      bits = 8
      intTy = llvmIntType bits
      stringTy = llvmStringType
      testSumTy = (sumTypeName, [(intVariantName, intTy), (stringVariantName, stringTy)])
      sumType = PassTypes.SumType sumTypeName
      intValToAdd = 1
      intValToAddTo = 2
      intCase = llvmAddFunction bits intValToAdd
      valIfString = 5
      stringCase = llvmConstantFunction llvmStringType intTy $ Primitive.LitInt valIfString
      intSumVal = llvmAnnotatedTerm intTy $ llvmIntVal intValToAddTo
      stringSumVal = llvmAnnotatedTerm stringTy $ llvmStringVal "testString"
      intSumTerm = PassTypes.VariantM (sumTypeName, intVariantName, intSumVal)
      annotatedIntSumTerm = llvmAnnotatedTerm sumType intSumTerm
      stringSumTerm = PassTypes.VariantM (sumTypeName, stringVariantName, stringSumVal)
      annotatedStringSumTerm = llvmAnnotatedTerm sumType stringSumTerm
      intCaseTerm = PassTypes.MatchM (sumTypeName, annotatedIntSumTerm, [intCase, stringCase])
      annotatedIntCaseTerm = llvmAnnotatedTerm intTy intCaseTerm
      stringCaseTerm = PassTypes.MatchM (sumTypeName, annotatedStringSumTerm, [intCase, stringCase])
      annotatedStringCaseTerm = llvmAnnotatedTerm intTy stringCaseTerm
      mulOp = annotatedBinOp ASTTypes.i8 Primitive.Mul
      mulTerm = annotatedBinOpApp intTy mulOp annotatedIntCaseTerm annotatedStringCaseTerm
      term = PassTypes.ScopedSumDeclM testSumTy mulTerm
      annotatedTerm = llvmAnnotatedTerm intTy term
      compiled = case Compilation.termLLVMToModule annotatedTerm of
        Left err -> P.error $ "LLVM sum types test compilation failed: " <> show err
        Right t -> t
  output <- runIntModule compiled
  let casted :: Int8 = fromIntegral output
  casted @?= fromIntegral ((intValToAdd + intValToAddTo) * valIfString)
