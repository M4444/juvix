module Juvix.Backends.LLVM.Pass.Types where

import qualified Juvix.Backends.LLVM.Primitive as Prim
import qualified Juvix.Core.Base.Types as BaseTypes
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

--------------------------------------------------------------------------------
-- LLVM internal term types
--------------------------------------------------------------------------------

-- Because core does not yet have algebraic types, we give LLVM
-- its own term type extended with algebraic types.

type RecordName = NameSymbol.T

type FieldName = NameSymbol.T

type SumName = NameSymbol.T

type VariantName = NameSymbol.T

-- | This is LLVM's internal analogue of Core's term type.
-- |
-- | Currently, the only difference is that LLVM's version has a
-- | RecordType and a SumType, as LLVM can now algebraic types, but Core can
-- | not yet type them.  Tests (and potentially other modules)
-- | can generate terms of TypeLLVM and pass them directly to
-- | LLVM to compile them even before Core can generate terms
-- | that LLVM would compile to RecordType or SumType.
data TypeLLVM
  = SymT NameSymbol.T
  | Star BaseTypes.Universe
  | PrimTy Prim.PrimTy
  | Pi Usage.T TypeLLVM TypeLLVM
  | Sig Usage.T TypeLLVM TypeLLVM
  | CatProduct TypeLLVM TypeLLVM
  | CatCoproduct TypeLLVM TypeLLVM
  | UnitTy
  | RecordType RecordName
  | SumType SumName
  deriving (Show, Read, Eq, Generic)

-- | When LLVM receives a term from Core, it converts the core type
-- | to its LLVM-extended version using this function (see the
-- | definition of TypeLLVM).
injectErasedTypeIntoLLVM :: ErasedAnn.Type Prim.PrimTy -> TypeLLVM
injectErasedTypeIntoLLVM t = case t of
  ErasedAnn.SymT name -> SymT name
  ErasedAnn.Star σ -> Star σ
  ErasedAnn.PrimTy ty -> PrimTy ty
  ErasedAnn.Pi π a b -> Pi π (injectErasedTypeIntoLLVM a) (injectErasedTypeIntoLLVM b)
  ErasedAnn.Sig π a b -> Sig π (injectErasedTypeIntoLLVM a) (injectErasedTypeIntoLLVM b)
  ErasedAnn.CatProduct a b -> CatProduct (injectErasedTypeIntoLLVM a) (injectErasedTypeIntoLLVM b)
  ErasedAnn.CatCoproduct a b -> CatCoproduct (injectErasedTypeIntoLLVM a) (injectErasedTypeIntoLLVM b)
  ErasedAnn.UnitTy -> UnitTy

--------------------------------------------------------------------------------
-- Closure Capture Forms
--------------------------------------------------------------------------------

data Capture = Capture
  { -- | @location@ represents where in the environment do we
    -- capture the value from.
    location :: CaptureFrom,
    -- | @slot@ represents the new slot that represents this
    -- captured value, for proper GEP generation.
    slot :: ArraySlot,
    -- | @capType@ represents the capture type of the captured
    -- argument
    capType :: TypeLLVM
  }
  deriving (Show)

----------------------------------------
-- Slot Location Storage Types
----------------------------------------

-- | @ArraySlot@ represents the slot in the array the node now is in
-- along with metadata associated with the slot.
data ArraySlot = Slot
  { -- | @previousName@ represents the old name in the source code,
    -- recorded for debugging purposes.
    previousName :: NameSymbol.T,
    -- | @newIndex@ represents the new slot location into the array.
    newIndex :: Index
  }
  deriving (Show)

-- | @Index@ is the index into the array
newtype Index = Index {num :: Natural} deriving (Show)

----------------------------------------
-- Storage Location and Indexing Types
----------------------------------------

data CaptureFrom
  = -- | @FromAmbientEnv@ represents the values that we capture having a
    -- proper name rather then being offsets into the environment.
    FromAmbientEnv NameSymbol.T
  | -- | @FromClosureEnv@ represents that the value that we are trying
    -- to store are offsets inside some environment, rather than a name
    -- that we can directly bind.
    FromClosureEnv IndexInto
  deriving (Show)

-- | @IndexInto@ represents the Index into an environment, along with
-- which environment it originates from.
data IndexInto = IndexInto
  { index :: Index,
    into :: FunctionEnvironment
  }
  deriving (Show)

-- | @FunctionEnvironment@ represents if the index into an array is
-- from the closure array/environment or the closure argument array/environment
data FunctionEnvironment
  = ClosureEnvironment
  | -- Currently unused due to how it's impossible to distinguish
    -- between functions and closures. We should in time compile with
    -- this, and will be needed for currying
    ArgumentEnvironemnt
  deriving (Show)

------------------------------
---- Record-related types ----
------------------------------

type FieldDecl = (FieldName, TypeLLVM)

type RecordSpec = [FieldDecl]

type RecordDecl = (RecordName, RecordSpec)

type TermFieldSelector = (RecordName, FieldName, Annotated TermLLVM)

type TermRecordConstructor = (RecordName, [Annotated TermLLVM])

---------------------------
---- Sum-related types ----
---------------------------

type VariantDecl = (VariantName, TypeLLVM)

type SumSpec = [VariantDecl]

type SumDecl = (SumName, SumSpec)

type TermVariantConstructor = (SumName, VariantName, Annotated TermLLVM)

type TermMatch = (SumName, Annotated TermLLVM, [Annotated TermLLVM])

--------------------------------------------------------------------------------
-- New Core Form we will Process Over
--------------------------------------------------------------------------------

data Annotated term = Ann
  { usage :: Usage.T,
    annTy :: TypeLLVM,
    term :: term
  }
  deriving (Show)

-- TODO ∷ replace more data types with SEXPs for easier
-- processing... Might end up with having more than 1 of these which
-- transformation is quite verbose
data TermLLVM
  = Var NameSymbol.T
  | -- index into a given closure
    ArrayIndex IndexInto
  | Prim Prim.RawPrimVal
  | -- Removing captures from LamM
    LamM
      { arguments :: [NameSymbol.T],
        body :: Annotated TermLLVM
      }
  | -- Addition to Core.
    Closure
      { capture :: [Capture],
        -- ArgumentOffsets are currently NameSymbol.T's due to calling
        -- conventions. Subject to change back to [ArraySlot]
        argumentOffsets :: [NameSymbol.T],
        body :: Annotated TermLLVM
      }
  | PairM
      (Annotated TermLLVM)
      (Annotated TermLLVM)
  | CatProductIntroM
      (Annotated TermLLVM)
      (Annotated TermLLVM)
  | CatProductElimLeftM
      (Annotated TermLLVM)
      (Annotated TermLLVM)
  | CatProductElimRightM
      (Annotated TermLLVM)
      (Annotated TermLLVM)
  | CatCoproductIntroLeftM (Annotated TermLLVM)
  | CatCoproductIntroRightM (Annotated TermLLVM)
  | CatCoproductElimM
      (Annotated TermLLVM)
      (Annotated TermLLVM)
      (Annotated TermLLVM)
      (Annotated TermLLVM)
      (Annotated TermLLVM)
  | UnitM
  | AppM
      (Annotated TermLLVM)
      [Annotated TermLLVM]
  | -- | One of the extensions to Core's term type, this wraps
    -- | a term in a record type declaration, so that the record
    -- | type is available within the wrapped term.
    ScopedRecordDeclM RecordDecl (Annotated TermLLVM)
  | -- | Another of the extensions to Core's term type, this selects
    -- | a field with the given name from a term which must have a
    -- | record type.  (So it's the eliminator for records.)
    FieldM TermFieldSelector
  | -- | Another of the extensions to Core's term type, this is the
    -- | introduction for records, which constructs a term of a record
    -- | type which must be in scope from terms which will become the
    -- | fields (whose types must therefore match those of the record type).
    RecordM TermRecordConstructor
  | -- | Another of the extensions to Core's term type, this wraps
    -- | a term in a sum type declaration, so that the sum
    -- | type is available within the wrapped term.
    ScopedSumDeclM SumDecl (Annotated TermLLVM)
  | -- | Another of the extensions to Core's term type, this constructs
    -- | a sum with the given name from a term which must have the type
    -- | of one of the sum's variants.  (So it's the introduction rule for
    -- | sums.)
    VariantM TermVariantConstructor
  | -- | Another of the extensions to Core's term type, this is the
    -- | elimination rule for sums, which destructs a term of a sum
    -- | type which must be in scope from terms which will become the
    -- | cases (whose types must therefore match those of the sum type).
    MatchM TermMatch
  deriving (Show)
