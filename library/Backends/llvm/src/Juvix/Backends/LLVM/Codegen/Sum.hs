module Juvix.Backends.LLVM.Codegen.Sum
  ( register,
    restoreTable,
    makeSum,
    makeCase,
    lookupType,
  )
where

import qualified Data.List as List
import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Closure as Closure
import qualified Juvix.Backends.LLVM.Codegen.Types as Types
import qualified Juvix.Backends.LLVM.Codegen.Types.Shared as Shared
import qualified Juvix.Backends.LLVM.Pass.Types as PassTypes
import Juvix.Library hiding (Type, local)
import qualified Juvix.Library.HashMap as Map
import Juvix.Library.NameSymbol as NameSymbol
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Type as Type
import qualified Safe.Exact as Exact
import qualified Prelude as P

-- | @register@ registers a sum type with the given name and
-- | variant names and types.  It returns the old sum table so
-- | that the caller can restore it (with restoreTable) after leaving
-- | the scope of a sum declaration.
register ::
  Types.Define m =>
  PassTypes.SumName ->
  -- | Variant names and types
  [(PassTypes.VariantName, Type.Type)] ->
  m Types.SumTable
register sumName variants = do
  oldTable <- get @"sumTab"
  llvmTypeName <- sumTypeName sumName
  Block.addType llvmTypeName llvmSumType
  let typeRef = Type.NamedTypeReference llvmTypeName
      variantDescs = zip (map toSymbol variantNames) llvmVariantTypes
  put @"sumTab" $ Map.insert (toSymbol sumName) (typeRef, variantDescs) oldTable
  pure oldTable
  where
  (variantNames, llvmVariantTypes) = unzip variants

restoreTable ::
  Types.Define m =>
  Types.SumTable ->
  m ()
restoreTable = put @"sumTab"

sumTypeName :: Types.Define m => NameSymbol.T -> m AST.Name
sumTypeName sumName =
  Block.internName <$> Block.generateUniqueSymbol taggedName
  where
  taggedName = "sum-" <> toSymbol sumName

indexBits :: Word32
indexBits = 32

indexType :: Type.Type
indexType = Type.i32

tagConstant :: Int -> Constant.Constant
tagConstant variantIndex =
  Constant.Int
    { Constant.integerBits = indexBits,
      Constant.integerValue = toInteger variantIndex
    }

tagOperand :: Int -> AST.Operand
tagOperand = AST.ConstantOperand . tagConstant

variantPtrType :: Type.Type
variantPtrType = Types.pointerOf Type.i8

llvmSumType :: Type.Type
llvmSumType =
  Type.StructureType
    { isPacked = True,
      -- We box types, both to allow recursive structures and also to avoid
      -- having to perform any recursive size calculations for now.
      -- Hence, the structure representing a sum contains a single
      -- pointer to point to a structure representing whichever variant
      -- the sum contains, and an index indicating which variant it is
      -- (by its index within the list of variants in the sum type
      -- declaration).
      elementTypes = [indexType, variantPtrType]
    }

getIndexPtr :: Types.Define m => AST.Operand -> m AST.Operand
getIndexPtr sumPtr =
  Block.getElementPtr
    Types.Minimal
      { type' = Types.pointerOf indexType,
        address' = sumPtr,
        indincies' = Block.constant32List [0, 0]
      }

getVariantPtr :: Types.Define m => AST.Operand -> m AST.Operand
getVariantPtr sumPtr =
  Block.getElementPtr
    Types.Minimal
      { type' = Types.pointerOf variantPtrType,
        address' = sumPtr,
        indincies' = Block.constant32List [0, 1]
      }

getSumDesc :: Types.LookupType m => PassTypes.SumName -> m Types.SumDesc
getSumDesc sumName = do
  sumTable <- get @"sumTab"
  case Map.lookup (toSymbol sumName) sumTable of
    Just sumDesc -> pure sumDesc
    Nothing ->
      throw @"err" $
        Types.NonExistentSumType "typechecker allowed sum of non-existent type"

lookupType :: Types.LookupType m => PassTypes.SumName -> m Type.Type
lookupType sumName = fst <$> getSumDesc sumName

oneArgFunctionType :: Type.Type -> Type.Type -> Type.Type
oneArgFunctionType arg result =
  Types.pointerOf
    Type.FunctionType
      { Type.resultType = result,
        Type.argumentTypes = [arg],
        Type.isVarArg = False
      }

makeCase ::
  Types.Define m =>
  -- | The name of the Sum type
  PassTypes.SumName ->
  -- | The otuput type
  Type.Type ->
  -- | The cased term
  AST.Operand ->
  -- | The (case type, case body, case env)
  [(Type.Type, AST.Operand, AST.Operand)] ->
  m AST.Operand
-- makeCase sumName outputType caseTypes term cases environments = do
makeCase sumName outputType term cases = do
  (sumType, variantDescs) <- getSumDesc sumName
  let values = map tagConstant [0 .. length variantDescs - 1]
      (variantNames, variantTypes ) = unzip variantDescs
      expectedCaseTypes = map (`oneArgFunctionType` outputType) variantTypes
  unless (caseTypes == expectedCaseTypes) $
      throw @"err" $
        Types.MismatchedCaseTypes $
          "typechecker allowed mismatched case types: expected "
            <> show expectedCaseTypes
            <> "; got "
            <> show caseTypes
            <> "; outputType "
            <> show outputType
            <> "; variantTypes "
            <> show variantTypes
  indexPtr <- getIndexPtr term
  index <- Block.load indexType indexPtr
  variantPtrLoc <- getVariantPtr term
  variantPtr <- Block.load variantPtrType variantPtrLoc
  let appliedCases =
        map
          ( \(ty, (caseFunc, environment)) -> do
              castedPtr <- Block.bitCast variantPtr (Types.pointerOf ty)
              variant <- Block.load ty castedPtr
              Block.call outputType caseFunc [(environment, []), (variant, [])]
          )
          (zip variantTypes $ zip caseBodies caseEnvs)
  Block.generateSwitch outputType index (Exact.zip3Exact variantNames values appliedCases) Nothing
  where
  (caseTypes, caseBodies, caseEnvs) = List.unzip3 cases

-- | Given the name of a sum type and a compiled variant term,
-- | allocate a sum type and store the given term in it.
-- |
-- | WARNING:  The memory allocations herein leak memory, pending our
-- | implementing garbage collection.
makeSum ::
  Types.Define m =>
  -- | Name of the sum type
  PassTypes.SumName ->
  -- | Name of the variant (constructor)
  PassTypes.VariantName ->
  Type.Type ->
  Type.Type ->
  AST.Operand ->
  m AST.Operand
makeSum sumName variantName compiledSumType compiledVariantType variantTerm = do
  (sumType, variantDescs) <- getSumDesc sumName
  unless (compiledSumType == sumType) $
      throw @"err" $
        Types.MismatchedSumTypes $
          "typechecker allowed mismatched sum types: expected "
            <> show sumType
            <> "; got "
            <> show compiledSumType
  variantIndex <- case List.findIndex (\desc -> fst desc == toSymbol variantName) variantDescs of
    Just index -> pure index
    Nothing ->
      throw @"err" $
        Types.NonExistentVariant "typechecker allowed selection of non-existent variant"
  let variantType = snd $ variantDescs P.!! variantIndex
  unless (compiledVariantType == variantType) $
      throw @"err" $
        Types.MismatchedVariantTypes $
          "typechecker allowed mismatched variant types: expected "
            <> show variantType
            <> "; got "
            <> show compiledVariantType
  -- Allocate the storage for the sum type structure, which contains a
  -- tag (which is an index into the variant list) and a pointer (to
  -- the memory allocated for the particular variant selected by the caller).
  sumPtr <- Block.mallocType sumType
  -- Store the index in the "tag" field of the sum type structure.
  indexPtr <- getIndexPtr sumPtr
  Block.store indexPtr $ tagOperand variantIndex
  -- Allocate the storage for the particular variant selected.
  newVariant <- Block.mallocType variantType
  -- Store the variant term passed in by the caller in the newly-allocated
  -- variant structure.
  Block.store newVariant variantTerm
  -- Store the pointer to the allocated variant in the variant-pointer
  -- field of the sum type structure.
  variantPtr <- getVariantPtr sumPtr
  Block.bitCast newVariant variantPtrType >>= Block.store variantPtr
  pure sumPtr
