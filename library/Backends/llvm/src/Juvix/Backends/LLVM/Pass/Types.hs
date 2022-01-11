module Juvix.Backends.LLVM.Pass.Types where

import Juvix.Backends.LLVM.Primitive as Prim
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
  deriving (Show, Read, Eq, Generic)

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
    capType :: ErasedAnn.Type Prim.PrimTy
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

--------------------------------------------------------------------------------
-- New Core Form we will Process Over
--------------------------------------------------------------------------------

data Annotated term = Ann
  { usage :: Usage.T,
    annTy :: ErasedAnn.Type Prim.PrimTy,
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
  deriving (Show)
