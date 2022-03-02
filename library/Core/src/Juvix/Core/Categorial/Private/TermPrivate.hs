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

data Shape carrier
  = InitialCat (HigherCategory carrier)
  | TerminalCat (HigherCategory carrier)
  | DiscretePair (HigherCategory carrier)
  | ParallelPair (HigherCategory carrier)
  | Span (HigherCategory carrier)
  | Cospan (HigherCategory carrier)
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
  = DirectedGraphCat (HigherCategory carrier)
  | IndexCat (Shape carrier)
  | OppositeCat (Category carrier)
  | ProductCat (Category carrier) (Category carrier)
  | FunctorCat (Category carrier) (Category carrier)
  | AdjunctionCat (HigherCategory carrier)
  | SliceCat (Object carrier)
  | CosliceCat (Object carrier)
  | RefinedADTCat (HigherCategory carrier)
  | HigherOrderRefinedADTCat (HigherCategory carrier)
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
  = -- | An CarrierObject is an object of the carrier category.
    CarrierObject carrier
  | FMapObject (Functor' carrier) (Object carrier)
  | -- | Any category is a "higher object" -- that is, an object of the category
    -- of all categories enriched over RefinedADTCategory.
    HigherObject (Category carrier)
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
  = CarrierMorphism (Maybe (carrier, carrier)) carrier
  | IdentityMorphism (Maybe (Object carrier))
  | ComposedMorphism (Morphism carrier) [Morphism carrier]
  | FMapMorphism (Functor' carrier) (Morphism carrier)
  | -- | Any functor is a "higher morphism" -- that is, an morphism of the
    -- category of all categories enriched over RefinedADTCategory.
    HigherMorphism (Functor' carrier)
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
  | ComposedFunctor (Functor' carrier) [Functor' carrier]
  | ConstFunctor (Object carrier)
  | DiagonalFunctor (Shape carrier) (Category carrier)
  | -- | The right adjoint of the diagonal functor with the same
    -- parameters.
    LimitFunctor (Shape carrier) (Category carrier)
  | -- | The left adjoint of the diagonal functor with the same
    -- parameter.
    ColimitFunctor (Shape carrier) (Category carrier)
  | FreeFunctor (Object carrier)
  | CofreeFunctor (Object carrier)
  | ForgetAlgebraFunctor (Object carrier)
  | ForgetCoalgebraFunctor (Object carrier)
  | CurryFunctor (Object carrier)
  | UncurryFunctor (Object carrier)
  | BaseChangeFunctor (Object carrier) (Object carrier)
  | DependentProductFunctor (Object carrier) (Object carrier)
  | DependentSumFunctor (Object carrier) (Object carrier)
  | CobaseChangeFunctor (Object carrier) (Object carrier)
  | CodependentProductFunctor (Object carrier) (Object carrier)
  | CodependentSumFunctor (Object carrier) (Object carrier)
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
  | ComposedAdjunction (Adjunction carrier) [Adjunction carrier]
  | LimitAdjunction (Shape carrier) (Category carrier)
  | ColimitAdjunction (Shape carrier) (Category carrier)
  | FreeForgetfulAlgebra (Object carrier)
  | ForgetfulCofreeAlgebra (Object carrier)
  | ProductHomAdjunction (Object carrier)
  | DependentSum (Object carrier)
  | DependentProduct (Object carrier)
  | CodependentSum (Object carrier)
  | CodependentProduct (Object carrier)
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

data AbstractTerm carrier
  = CategoryTerm (Category carrier)
  | ObjectTerm (Object carrier)
  | MorphismTerm (Morphism carrier)
  | FunctorTerm (Functor' carrier)
  | AdjunctionTerm (Adjunction carrier)
  | HigherCategoryTerm (HigherCategory carrier)
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

FunctorTemplates.makeBaseFunctor ''Category

type instance Foldable.Base (Category a) = CategoryF a

FunctorTemplates.makeBaseFunctor ''Object

type instance Foldable.Base (Object a) = ObjectF a

FunctorTemplates.makeBaseFunctor ''Morphism

type instance Foldable.Base (Morphism a) = MorphismF a

FunctorTemplates.makeBaseFunctor ''Functor'

type instance Foldable.Base (Functor' a) = Functor'F a

FunctorTemplates.makeBaseFunctor ''Adjunction

type instance Foldable.Base (Adjunction a) = AdjunctionF a

FunctorTemplates.makeBaseFunctor ''HigherCategory

type instance Foldable.Base (HigherCategory a) = HigherCategoryF a

FunctorTemplates.makeBaseFunctor ''AbstractTerm

type instance Foldable.Base (AbstractTerm a) = AbstractTermF a

FunctorTemplates.makeBaseFunctor ''Term

type instance Foldable.Base (Term a) = TermF a
