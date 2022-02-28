{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module Juvix.Core.Categorial.Private.TermPrivate where

import qualified Data.Aeson as Aeson
import qualified Data.Functor.Foldable as Foldable
import qualified Data.Functor.Foldable.TH as FunctorTemplates
import Juvix.Library
  ( Bounded,
    Data,
    Enum,
    Eq,
    Foldable,
    Functor,
    Generic,
    Hashable,
    Maybe,
    NFData,
    Ord,
    Read,
    Show,
    Traversable,
    Typeable,
  )
import qualified Juvix.Sexp.Serialize as Serialize
import qualified Juvix.Sexp.Types as SexpTypes

type AllInstanceAlgebra freeAlgObj =
  ( Read freeAlgObj,
    Show freeAlgObj,
    Eq freeAlgObj,
    Hashable freeAlgObj,
    Ord freeAlgObj,
    Generic freeAlgObj,
    Typeable freeAlgObj,
    Data freeAlgObj,
    NFData freeAlgObj,
    Aeson.ToJSON freeAlgObj,
    Aeson.FromJSON freeAlgObj,
    Aeson.ToJSONKey freeAlgObj,
    Aeson.FromJSONKey freeAlgObj,
    Serialize.DefaultOptions freeAlgObj,
    Serialize.Serialize freeAlgObj
  )

type MinimalInstanceAlgebra freeAlgObj =
  ( Show freeAlgObj,
    Eq freeAlgObj
  )

data Keyword
  = KRefinedADTCat
  | KAlgObject
  | KFreeAlgMorphism
  deriving
    ( Read,
      Show,
      Eq,
      Hashable,
      Enum,
      Bounded,
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

data Symbol freeAlgObj
  = Keyword Keyword
  | Variable freeAlgObj
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

data Object freeAlgObj
  = -- | Any category is a "higher object" -- that is, an object of the category
    -- of all categories enriched over RefinedADTCategory.
    HigherObject (Category freeAlgObj)
  | -- | An AlgebraObject is an object of the carrier category.
    AlgebraObject freeAlgObj
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

data Category freeAlgObj
  = DirectedGraphCat
  | InitialCat
  | TerminalCat
  | ProductCat (Category freeAlgObj) (Category freeAlgObj)
  | OppositeCat (Category freeAlgObj)
  | SliceCat (Object freeAlgObj)
  | CosliceCat (Object freeAlgObj)
  | FunctorCat (Category freeAlgObj) (Category freeAlgObj)
  | RefinedADTCat
  | HigherOrderRefinedADTCat
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

data Annotation freeAlgObj = Annotation freeAlgObj freeAlgObj
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

data UnannotatedMorphism freeAlgObj
  = FreeAlgMorphism freeAlgObj
  | Composition [Morphism freeAlgObj]
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

data Morphism freeAlgObj
  = Morphism (UnannotatedMorphism freeAlgObj) (Maybe (Annotation freeAlgObj))
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

data Functor' freeAlgObj
  = IdentityFunctor (Category freeAlgObj)
  | ComposeFunctors (Functor' freeAlgObj) (Functor' freeAlgObj)
  | -- | The diagonal functor from the parameter category to its
    -- product with itself.
    DiagonalFunctor (Category freeAlgObj)
  | -- | The right adjoint of the diagonal functor with the same
    -- parameter.
    ProductFunctor (Category freeAlgObj)
  | -- | The left adjoint of the diagonal functor with the same
    -- parameter.
    CoproductFunctor (Category freeAlgObj)
  | LeftFunctor (Functor' freeAlgObj)
  | RightFunctor (Functor' freeAlgObj)
  | ConstFunctor (Object freeAlgObj)
  | FreeFunctor (Object freeAlgObj)
  | CofreeFunctor (Object freeAlgObj)
  | ForgetAlgebraFunctor (Object freeAlgObj)
  | ProductExponentialFunctor (Object freeAlgObj)
  | BaseChangeFunctor (Object freeAlgObj) (Object freeAlgObj)
  | CobaseChangeFunctor (Object freeAlgObj) (Object freeAlgObj)
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

data NaturalTransformation freeAlgObj
  = IdentityNaturalTransformation (Functor' freeAlgObj)
  | Substitution (Functor' freeAlgObj) (Functor' freeAlgObj)
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

data Adjunction freeAlgObj
  = IdentityAdjunction (Category freeAlgObj)
  | ComposeAdjunctions (Adjunction freeAlgObj) (Adjunction freeAlgObj)
  | ProductAdjunction (Category freeAlgObj)
  | CoproductAdjunction (Category freeAlgObj)
  | SliceAdjunction (Object freeAlgObj)
  | CosliceAdjunction (Object freeAlgObj)
  | FreeAlgebraAdjunction (Object freeAlgObj)
  | CofreeAlgebraAdjunction (Object freeAlgObj)
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

data HigherCategory freeAlgObj
  = MinimalMetalogic
  | -- | Interpret a category as a higher category by specifying
    -- an adjunction category.
    FromCategory (Category freeAlgObj) (Category freeAlgObj)
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

FunctorTemplates.makeBaseFunctor ''Object

type instance Foldable.Base (Object a) = ObjectF a

FunctorTemplates.makeBaseFunctor ''Category

type instance Foldable.Base (Category a) = CategoryF a

FunctorTemplates.makeBaseFunctor ''UnannotatedMorphism

type instance Foldable.Base (UnannotatedMorphism a) = UnannotatedMorphismF a

FunctorTemplates.makeBaseFunctor ''Morphism

type instance Foldable.Base (Morphism a) = MorphismF a

FunctorTemplates.makeBaseFunctor ''Functor'

type instance Foldable.Base (Functor' a) = Functor'F a

FunctorTemplates.makeBaseFunctor ''NaturalTransformation

type instance Foldable.Base (NaturalTransformation a) = NaturalTransformationF a

FunctorTemplates.makeBaseFunctor ''Adjunction

type instance Foldable.Base (Adjunction a) = AdjunctionF a

FunctorTemplates.makeBaseFunctor ''HigherCategory

type instance Foldable.Base (HigherCategory a) = HigherCategoryF a

data AbstractTerm freeAlgObj
  = HigherCategoryTerm (HigherCategory freeAlgObj)
  | CategoryTerm (Category freeAlgObj)
  | ObjectTerm (Object freeAlgObj)
  | MorphismTerm (Morphism freeAlgObj)
  | FunctorTerm (Functor' freeAlgObj)
  | NaturalTransformationTerm (NaturalTransformation freeAlgObj)
  | AdjunctionTerm (Adjunction freeAlgObj)
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

FunctorTemplates.makeBaseFunctor ''AbstractTerm

type instance Foldable.Base (AbstractTerm a) = AbstractTermF a

type ConcreteTerm freeAlgObj = SexpTypes.Base (Symbol freeAlgObj)

data Term freeAlgObj
  = RepresentedTerm (AbstractTerm freeAlgObj)
  | SexpRepresentation (ConcreteTerm freeAlgObj)
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
      Serialize.Serialize,
      Functor,
      Foldable,
      Traversable
    )

FunctorTemplates.makeBaseFunctor ''Term

type instance Foldable.Base (Term a) = TermF a
