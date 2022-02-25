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
  | DecodeUnimplemented (TermPrivate.ConcreteTerm freeAlgObj) Text
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
  | CodegenUnimplemented (TermPrivate.AbstractTerm freeAlgObj) Text
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
  = EvalUnimplemented (TermPrivate.Term freeAlgObj) Text
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
  = EraseUnimplemented (TermPrivate.AbstractTerm freeAlgObj) Text
  | ErasingUncheckedTerm (TermPrivate.Term freeAlgObj)
  | AlreadyErasedMorphism (TermPrivate.Morphism freeAlgObj)
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