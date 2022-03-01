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
import Juvix.Core.Categorial.Private.TermPrivate
  ( AbstractTerm,
    Adjunction,
    Category,
    ConcreteTerm,
    Functor',
    Keyword,
    Morphism,
    Symbol,
    Term,
  )
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
  = CheckUnimplemented (AbstractTerm carrier) Text
  | AlreadyCheckedTerm (AbstractTerm carrier)
  | IllFormedSExpression (ConcreteTerm carrier)
  | InvalidTermRepresentation (ConcreteTerm carrier)
  | InvalidAtom (ConcreteTerm carrier) (SexpTypes.Atom (Symbol carrier))
  | EmptySexp
  | NonEliminatableTerm (AbstractTerm carrier)
  | IllegalFunctorComposition (Functor' carrier) (Functor' carrier)
  | FunctorAcrossHigherCategories (Functor' carrier)
  | WrongNumberOfArgumentsForKeyword Keyword
  | KeywordRequiresArguments Keyword
  | ExpectedAlgebraTerm (ConcreteTerm carrier)
  | IllegalMorphismComposition (Morphism carrier) (Morphism carrier)
  | ProjectingNonProductFunctor (Functor' carrier)
  | CheckingErasedMorphism (Morphism carrier)
  | HigherCategoryMismatch (Category carrier) (Category carrier)
  | IllegalAdjunctionComposition (Adjunction carrier) (Adjunction carrier)
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
  = CodegenUnchecked (ConcreteTerm carrier)
  | CodegenErased (AbstractTerm carrier)
  | CodegenUnimplemented (AbstractTerm carrier) Text
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
  | ConsOfRepresentedTerm (AbstractTerm carrier)
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
  = ErasingUncheckedTerm (Term carrier)
  | AlreadyErasedMorphism (Morphism carrier)
  | EraseUnimplemented (AbstractTerm carrier) Text
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
