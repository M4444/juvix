{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Representation of LLVM primitives in Juvix.
module Juvix.Backends.LLVM.Primitive
  ( PrimTy (..),
    arityTy,
    RawPrimVal (..),
    PrimVal,
    CompilationError (..),
    arityRaw,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Sexp.Serialize as Serialize
import qualified LLVM.AST.AddrSpace as AddrSpace
import qualified LLVM.AST.Name as LLVMName
import qualified LLVM.AST.Type as LLVM

-- | Intermediate representation of types.
deriving anyclass instance NFData AddrSpace.AddrSpace

deriving anyclass instance NFData LLVM.FloatingPointType

deriving anyclass instance NFData LLVMName.Name

deriving anyclass instance NFData LLVM.Type

deriving anyclass instance Hashable AddrSpace.AddrSpace

deriving anyclass instance Hashable LLVM.FloatingPointType

deriving anyclass instance Hashable LLVMName.Name

deriving anyclass instance Hashable LLVM.Type

deriving anyclass instance Serialize.DefaultOptions AddrSpace.AddrSpace

deriving anyclass instance Serialize.DefaultOptions LLVM.FloatingPointType

deriving anyclass instance Serialize.DefaultOptions LLVMName.Name

deriving anyclass instance Serialize.DefaultOptions LLVM.Type

deriving anyclass instance Serialize.Serialize AddrSpace.AddrSpace

deriving anyclass instance Serialize.Serialize LLVM.FloatingPointType

deriving anyclass instance Serialize.Serialize LLVMName.Name

deriving anyclass instance Serialize.Serialize LLVM.Type

data PrimTy
  = -- | TODO: Rely on the LLVM-defined types for now.
    PrimTy LLVM.Type
  | Set
  deriving anyclass
    ( Typeable,
      NFData,
      Hashable,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )
  deriving stock
    ( Read,
      Show,
      Eq,
      Ord,
      Generic,
      Data
    )

-- | TODO: A placeholder arity implementation for types.
arityTy :: PrimTy -> Natural
arityTy _ = 0

-- | Raw representation of some primitives of LLVM.
data RawPrimVal
  = Add
  | Sub
  | Mul
  | LitInt Integer
  | LitString Text
  deriving anyclass
    ( Typeable,
      NFData,
      Hashable,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )
  deriving stock
    ( Generic,
      Data,
      Read,
      Show,
      Eq,
      Ord
    )

-- | The primitive values as exposed to users of Juvix, wrapping inside a
-- return or a continuation.
type PrimVal ext = App.Return' ext (Param.PrimType PrimTy) RawPrimVal

-- | Custom compilation errors.
data CompilationError
  = -- | TODO: Just a placeholder for now.
    PlaceHolderError
  deriving anyclass
    ( Typeable,
      NFData,
      Hashable,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )
  deriving stock
    ( Generic,
      Data,
      Read,
      Show,
      Eq,
      Ord
    )

-- | Arity of `RawPrimVal`.
arityRaw :: RawPrimVal -> Natural
arityRaw p = case p of
  Add -> 2
  Mul -> 2
  Sub -> 2
  LitInt {} -> 0
  LitString {} -> 0
