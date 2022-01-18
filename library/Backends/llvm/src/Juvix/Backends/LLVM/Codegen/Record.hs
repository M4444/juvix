module Juvix.Backends.LLVM.Codegen.Record
  ( register,
    loadField,
    makeRecord,
  )
where

import qualified Data.List as List
import qualified Juvix.Backends.LLVM.Codegen.Block as Block
import qualified Juvix.Backends.LLVM.Codegen.Types as Types
import qualified Juvix.Backends.LLVM.Pass.Types as PassTypes
import Juvix.Library hiding (Type, local)
import qualified Juvix.Library.HashMap as Map
import Juvix.Library.NameSymbol as NameSymbol
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as Type
import qualified Prelude as P

-- | @register@ registers a record type with the given name and
-- | field names and types
register ::
  Types.Define m =>
  PassTypes.RecordName ->
  [PassTypes.FieldName] ->
  [Type.Type] ->
  m ()
register recordName fieldNames llvmFieldTypes = do
  let llvmTypeName = recordTypeName recordName
  Block.addType llvmTypeName (llvmRecordType llvmFieldTypes)
  let typeRef = Type.NamedTypeReference llvmTypeName
  let fieldDescs = zip (map toSymbol fieldNames) llvmFieldTypes
  modify @"recordTab" $ Map.insert (toSymbol recordName) (typeRef, fieldDescs)

recordTypeName :: NameSymbol.T -> AST.Name
recordTypeName recordName = Block.internName $ "record-" <> toSymbol recordName

llvmRecordType :: [Type.Type] -> Type.Type
llvmRecordType llvmFieldTypes =
  Type.StructureType
    { isPacked = True,
      elementTypes =
        -- We box types, both to allow recursive structures and also to avoid
        -- having to perform any recursive size calculations for now.
        map Types.pointerOf llvmFieldTypes
    }

-- | Get the names and types of the fields of the record type with the given name.
recordFields :: Types.Define m => PassTypes.RecordName -> m [(Symbol, Type.Type)]
recordFields recordName = do
  recordTable <- get @"recordTab"
  case Map.lookup (toSymbol recordName) recordTable of
    Just (_, fields) -> pure fields
    Nothing ->
      throw @"err" $
        Types.NonExistentRecordType "typechecker allowed record of non-existent type"

-- | Get the index of the field with the given name within the list of
-- | fields of the record with the given name.
fieldIndex ::
  Types.Define m =>
  PassTypes.RecordName ->
  PassTypes.FieldName ->
  m Int
fieldIndex recordName fieldName = do
  fieldDescs <- recordFields recordName
  case List.elemIndex (toSymbol fieldName) $ map fst fieldDescs of
    Just index -> pure index
    Nothing ->
      throw @"err" $
        Types.NonExistentField "typechecker allowed selection of non-existent field"

-- | Given the name of a record type and a pointer to the location of
-- | a record term with that type, get a pointer to the location of
-- | the pointer to the field with the given name inside the record.
getFieldPtr ::
  Types.Define m =>
  PassTypes.RecordName ->
  PassTypes.FieldName ->
  AST.Operand ->
  m AST.Operand
getFieldPtr recordName fieldName location = do
  fieldDescs <- recordFields recordName
  index <- fieldIndex recordName fieldName
  Block.getElementPtr
    Types.Minimal
      { type' = Types.pointerOf $ Types.pointerOf $ snd $ fieldDescs P.!! index,
        address' = location,
        indincies' = Block.constant32List [0, toInteger index]
      }

-- | Given the name of a record type, a pointer to the location of
-- | a record term with that type, the name of a field in the record,
-- | and an LLVM type, load the field with the given name, treating
-- | it as the given LLVM type.
loadField ::
  Types.Define m =>
  PassTypes.RecordName ->
  PassTypes.FieldName ->
  AST.Operand ->
  Type.Type ->
  m AST.Operand
loadField recordName fieldName location ty = do
  fieldDescs <- recordFields recordName
  index <- fieldIndex recordName fieldName
  field <-
    Block.loadElementPtr
      Types.Minimal
        { type' = Types.pointerOf $ snd $ fieldDescs P.!! index,
          address' = location,
          indincies' = Block.constant32List [0, toInteger index]
        }
  Block.load ty field

-- | Given the name of a record type and a list of compiled field terms,
-- | allocate a record and store the given terms in it.
makeRecord ::
  Types.Define m =>
  PassTypes.RecordName ->
  [AST.Operand] ->
  m AST.Operand
makeRecord recordName fieldTerms = do
  recordTable <- get @"recordTab"
  record <-
    case Map.lookup (toSymbol recordName) recordTable of
      Just recordDesc -> pure recordDesc
      Nothing ->
        throw @"err" $
          Types.NonExistentRecordType "typechecker allowed record of non-existent type"
  let (recordType, fieldDescs) = record
  recordPtr <- Block.mallocType recordType
  let fieldNames = map (fromSymbol . fst) fieldDescs
  let fieldTypes = map snd fieldDescs
  fieldPtrs <- mapM (flip (getFieldPtr recordName) recordPtr) fieldNames
  newFields <- mapM Block.mallocType fieldTypes
  -- Store the pointers to the memory newly allocated for the fields into
  -- the pointers in the newly-allocated record structure.
  Block.mapStore fieldPtrs newFields
  -- Store the passed-in terms in the newly-allocated fields.
  Block.mapStore newFields fieldTerms
  pure recordPtr
