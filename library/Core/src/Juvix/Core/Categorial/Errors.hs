{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module Juvix.Core.Categorial.Errors
  ( CheckError (..),
    CodegenError (..),
    SyntaxError (..),
    EvalError (..),
    EraseError (..),
  )
where

import qualified Data.Aeson as Aeson
import qualified Juvix.Core.Categorial.Private.TermPrivate as TermPrivate
import Juvix.Library
  ( Data,
    Eq,
    Foldable,
    Functor,
    Generic,
    Hashable,
    NFData,
    Ord,
    Read,
    Show,
    Text,
    Traversable,
    Typeable,
  )
import qualified Juvix.Sexp.Serialize as Serialize
import qualified Juvix.Sexp.Types as SexpTypes

data CheckError carrier
  = CheckUnimplemented (TermPrivate.AbstractTerm carrier) Text
  | AlreadyCheckedTerm (TermPrivate.AbstractTerm carrier)
  | IllFormedSExpression (TermPrivate.ConcreteTerm carrier)
  | InvalidTermRepresentation (TermPrivate.ConcreteTerm carrier)
  | InvalidAtom
      (TermPrivate.ConcreteTerm carrier)
      (SexpTypes.Atom (TermPrivate.Symbol carrier))
  | EmptySexp
  | NonEliminatableTerm (TermPrivate.AbstractTerm carrier)
  | IllegalFunctorComposition
      (TermPrivate.Functor' carrier)
      (TermPrivate.Functor' carrier)
  | WrongNumberOfArgumentsForKeyword TermPrivate.Keyword
  | KeywordRequiresArguments TermPrivate.Keyword
  | ExpectedAlgebraTerm (TermPrivate.ConcreteTerm carrier)
  | CheckingMorphismAfterErasure (TermPrivate.UnannotatedMorphism carrier)
  | IdentityBetweenDifferentObjects carrier carrier
  | IllTypedMorphismComposition
      (TermPrivate.UnannotatedMorphism carrier)
      carrier
      carrier
  | ProjectingNonProductFunctor (TermPrivate.Functor' carrier)
  deriving
    ( Read,
      Show,
      Eq,
      Ord,
      Generic,
      Typeable,
      Data,
      Functor,
      Foldable,
      Traversable,
      NFData,
      Hashable,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )

data CodegenError carrier
  = CodegenUnchecked (TermPrivate.ConcreteTerm carrier)
  | CodegenErased (TermPrivate.AbstractTerm carrier)
  deriving
    ( Read,
      Show,
      Eq,
      Ord,
      Generic,
      Typeable,
      Data,
      Functor,
      Foldable,
      Traversable,
      NFData,
      Hashable,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )

data SyntaxError carrier
  = NoSuchKeyword Text
  | ConsOfRepresentedTerm (TermPrivate.AbstractTerm carrier)
  deriving
    ( Read,
      Show,
      Eq,
      Ord,
      Generic,
      Typeable,
      Data,
      Functor,
      Foldable,
      Traversable,
      NFData,
      Hashable,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )

data EvalError carrier
  = EvalError
  deriving
    ( Read,
      Show,
      Eq,
      Ord,
      Generic,
      Typeable,
      Data,
      Functor,
      Foldable,
      Traversable,
      NFData,
      Hashable,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )

data EraseError carrier
  = ErasingUncheckedTerm (TermPrivate.Term carrier)
  | AlreadyErasedMorphism (TermPrivate.UnannotatedMorphism carrier)
  deriving
    ( Read,
      Show,
      Eq,
      Ord,
      Generic,
      Typeable,
      Data,
      Functor,
      Foldable,
      Traversable,
      NFData,
      Hashable,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )
