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

type AllInstanceAlgebra carrier =
  ( Read carrier,
    Show carrier,
    Eq carrier,
    Hashable carrier,
    Ord carrier,
    Generic carrier,
    Typeable carrier,
    Data carrier,
    NFData carrier,
    Aeson.ToJSON carrier,
    Aeson.FromJSON carrier,
    Aeson.ToJSONKey carrier,
    Aeson.FromJSONKey carrier,
    Serialize.DefaultOptions carrier,
    Serialize.Serialize carrier
  )

type MinimalInstanceAlgebra carrier =
  ( Show carrier,
    Eq carrier
  )

data Keyword
  = KRefinedADTCat
  | KCarrierObject
  | KCarrierMorphism
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

data Symbol carrier
  = Keyword Keyword
  | Variable carrier
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

data Object carrier
  = -- | Any category is a "higher object" -- that is, an object of the category
    -- of all categories enriched over RefinedADTCategory.
    HigherObject (Category carrier)
  | -- | An CarrierObject is an object of the carrier category.
    CarrierObject carrier
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

data Category carrier
  = DirectedGraphCat (Object carrier)
  | InitialCat
  | TerminalCat
  | ProductCat (Category carrier) (Category carrier)
  | OppositeCat (Category carrier)
  | SliceCat (Object carrier)
  | CosliceCat (Object carrier)
  | FunctorCat (Category carrier) (Category carrier)
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

data Annotation carrier = Annotation carrier carrier
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

data UnannotatedMorphism carrier
  = CarrierMorphism carrier
  | Composition [Morphism carrier]
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

data Morphism carrier
  = Morphism (UnannotatedMorphism carrier) (Maybe (Annotation carrier))
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

data Functor' carrier
  = IdentityFunctor (Category carrier)
  | ComposeFunctors (Functor' carrier) (Functor' carrier)
  | -- | The diagonal functor from the parameter category to its
    -- product with itself.
    DiagonalFunctor (Category carrier)
  | -- | The right adjoint of the diagonal functor with the same
    -- parameter.
    ProductFunctor (Category carrier)
  | -- | The left adjoint of the diagonal functor with the same
    -- parameter.
    CoproductFunctor (Category carrier)
  | LeftFunctor (Functor' carrier)
  | RightFunctor (Functor' carrier)
  | ConstFunctor (Object carrier)
  | FreeFunctor (Object carrier)
  | CofreeFunctor (Object carrier)
  | ForgetAlgebraFunctor (Object carrier)
  | ProductExponentialFunctor (Object carrier)
  | BaseChangeFunctor (Object carrier) (Object carrier)
  | CobaseChangeFunctor (Object carrier) (Object carrier)
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

data NaturalTransformation carrier
  = IdentityNaturalTransformation (Functor' carrier)
  | Substitution (Functor' carrier) (Functor' carrier)
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

data Adjunction carrier
  = IdentityAdjunction (Category carrier)
  | ComposeAdjunctions (Adjunction carrier) (Adjunction carrier)
  | ProductAdjunction (Category carrier)
  | CoproductAdjunction (Category carrier)
  | SliceAdjunction (Object carrier)
  | CosliceAdjunction (Object carrier)
  | FreeAlgebraAdjunction (Object carrier)
  | CofreeAlgebraAdjunction (Object carrier)
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

data HigherCategory carrier
  = MinimalMetalogic
  | -- | Interpret a category as a higher category by specifying
    -- an adjunction category.
    FromCategory (Category carrier) (Category carrier)
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

data AbstractTerm carrier
  = HigherCategoryTerm (HigherCategory carrier)
  | CategoryTerm (Category carrier)
  | ObjectTerm (Object carrier)
  | MorphismTerm (Morphism carrier)
  | FunctorTerm (Functor' carrier)
  | NaturalTransformationTerm (NaturalTransformation carrier)
  | AdjunctionTerm (Adjunction carrier)
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

type ConcreteTerm carrier = SexpTypes.Base (Symbol carrier)

data Term carrier
  = RepresentedTerm (AbstractTerm carrier)
  | SexpRepresentation (ConcreteTerm carrier)
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
