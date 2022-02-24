{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

module Juvix.Backends.Interpreter.Primitive
  ( PrimTy (..),
    arityTy,
    RawPrimVal (..),
    PrimVal,
    CompilationError (..),
    arityRaw,
  )
where

import qualified Data.Aeson as Aeson
import qualified GHC.Natural as Natural
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
  ( Bool,
    Bounded,
    Data,
    Double,
    Enum,
    Eq,
    Generic,
    Hashable,
    Integer,
    NFData,
    Natural,
    Ord,
    Read,
    Show,
    Text,
    Typeable,
  )
import qualified Juvix.Sexp.Serialize as Serialize
import qualified Prelude

data PrimTy
  = PrimBool
  | PrimNat
  | PrimInt
  | PrimFloat
  | PrimString
  | PrimList
  | PrimSexp
  | PrimFunc
  deriving
    ( Read,
      Show,
      Eq,
      Hashable,
      Ord,
      Enum,
      Bounded,
      Generic,
      Typeable,
      Data,
      NFData,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )

-- | TODO: A placeholder arity implementation for types.
arityTy :: PrimTy -> Natural
arityTy PrimBool = 0
arityTy PrimNat = 0
arityTy PrimInt = 0
arityTy PrimFloat = 0
arityTy PrimString = 0
arityTy PrimList = 1
arityTy PrimSexp = 1
arityTy PrimFunc = 2

-- | Raw representation of some primitives of Interpreter.
data RawPrimVal
  = BoolVal Bool
  | NatVal Natural
  | IntVal Integer
  | FloatVal Double
  | StringVal Text
  | ListNil
  | ListCons
  | SexpNil
  | SexpAtom
  | SexpCons
  | PrimIdentity
  | PrimCompose
  | PrimIntToNat
  | PrimAdd
  | PrimMul
  | PrimAppend
  | PrimNth
  | PrimLength
  | PrimIntEq
  | PrimIntLt
  | PrimStringEq
  | PrimIntIf
  | PrimStringIf
  | PrimAnd
  | PrimOr
  | PrimNot
  deriving
    ( Read,
      Show,
      Eq,
      Hashable,
      Ord,
      Generic,
      Typeable,
      Data,
      NFData,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )

-- | The primitive values as exposed to users of Juvix, wrapping inside a
-- return or a continuation.
type PrimVal ext = ()

-- | Custom compilation errors.
newtype CompilationError = CompilationError ()
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
arityRaw p = Prelude.error "Interpreter: arityRaw not yet implemented"
