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

data CheckError freeAlgObj
  = CheckUnimplemented (TermPrivate.AbstractTerm freeAlgObj) Text
  | AlreadyCheckedTerm (TermPrivate.AbstractTerm freeAlgObj)
  | IllFormedSExpression (TermPrivate.ConcreteTerm freeAlgObj)
  | InvalidTermRepresentation (TermPrivate.ConcreteTerm freeAlgObj)
  | InvalidAtom
      (TermPrivate.ConcreteTerm freeAlgObj)
      (SexpTypes.Atom (TermPrivate.Symbol freeAlgObj))
  | EmptySexp
  | NonEliminatableTerm (TermPrivate.AbstractTerm freeAlgObj)
  | IllegalFunctorComposition
      (TermPrivate.Functor' freeAlgObj)
      (TermPrivate.Functor' freeAlgObj)
  | WrongNumberOfArgumentsForKeyword TermPrivate.Keyword
  | KeywordRequiresArguments TermPrivate.Keyword
  | ExpectedAlgebraTerm (TermPrivate.ConcreteTerm freeAlgObj)
  | CheckingMorphismAfterErasure (TermPrivate.UnannotatedMorphism freeAlgObj)
  | IdentityBetweenDifferentObjects freeAlgObj freeAlgObj
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

data CodegenError freeAlgObj
  = CodegenUnchecked (TermPrivate.ConcreteTerm freeAlgObj)
  | CodegenErased (TermPrivate.AbstractTerm freeAlgObj)
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

data SyntaxError freeAlgObj
  = NoSuchKeyword Text
  | ConsOfRepresentedTerm (TermPrivate.AbstractTerm freeAlgObj)
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

data EvalError freeAlgObj
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

data EraseError freeAlgObj
  = ErasingUncheckedTerm (TermPrivate.Term freeAlgObj)
  | AlreadyErasedMorphism (TermPrivate.UnannotatedMorphism freeAlgObj)
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
