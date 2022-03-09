module Juvix.Core.Categorial.Private.TermLibrary where

import Juvix.Core.Categorial.Errors as CategorialErrors ()
import Juvix.Core.Categorial.Private.TermPrivate
  ( AbstractTerm (..),
    Adjunction (..),
    Category (..),
    Functor' (..),
    HigherCategory (..),
    Morphism (..),
    Object (..),
    Shape (..),
  )
import Juvix.Core.Categorial.Private.Utils ()
import Juvix.Library
  ( ($),
  )

minimalEmptyIndex :: Shape a
minimalEmptyIndex = EmptyIndex MinimalMetalogic

minimalSingletonIndex :: Shape a
minimalSingletonIndex = SingletonIndex MinimalMetalogic

singletonVertex :: Object a
singletonVertex = Vertex minimalSingletonIndex 0

emptyIndex :: Category a
emptyIndex = IndexCat minimalEmptyIndex

singletonIndex :: Category a
singletonIndex = IndexCat minimalSingletonIndex

initialCat :: AbstractTerm a
initialCat = CategoryTerm emptyIndex

terminalCat :: AbstractTerm a
terminalCat = CategoryTerm singletonIndex

terminalCatObj :: AbstractTerm a
terminalCatObj = ObjectTerm singletonVertex

solCat :: Category a
solCat = RefinedADTCat MinimalMetalogic

holCat :: Category a
holCat = HigherOrderRefinedADTCat MinimalMetalogic

solTerm :: AbstractTerm a
solTerm = CategoryTerm solCat

holTerm :: AbstractTerm a
holTerm = CategoryTerm holCat

toTerminalFunctor :: Functor' a
toTerminalFunctor = ConstFunctor singletonVertex

holInitialAdjunction :: Adjunction a
holInitialAdjunction = InitialAdjunction holCat

initialFunctor :: Functor' a
initialFunctor = LeftAdjoint holInitialAdjunction

holVoid :: Object a
holVoid = FMapObject initialFunctor singletonVertex

holVoidTerm :: AbstractTerm a
holVoidTerm = ObjectTerm holVoid

holFromVoid :: Object a -> Morphism a
holFromVoid = AdjunctionCounit holInitialAdjunction

holTerminalAdjunction :: Adjunction a
holTerminalAdjunction = TerminalAdjunction holCat

terminalFunctor :: Functor' a
terminalFunctor = RightAdjoint holTerminalAdjunction

holUnit :: Object a
holUnit = FMapObject terminalFunctor singletonVertex

holUnitTerm :: AbstractTerm a
holUnitTerm = ObjectTerm holUnit

holToUnit :: Object a -> Morphism a
holToUnit = AdjunctionUnit holTerminalAdjunction

discretePair :: Shape a
discretePair = DiscretePair MinimalMetalogic

discretePairIndex :: Category a
discretePairIndex = IndexCat discretePair

discretePairCat :: AbstractTerm a
discretePairCat = CategoryTerm discretePairIndex

holDiagonal :: Functor' a
holDiagonal = DiagonalFunctor discretePair holCat

holProductObj :: Object a -> Object a -> Object a
holProductObj x y =
  FMapObject
    (RightAdjoint $ LimitAdjunction discretePair holCat)
    (ProductObject discretePair [x, y])

holCoproductObj :: Object a -> Object a -> Object a
holCoproductObj x y =
  FMapObject
    (LeftAdjoint $ ColimitAdjunction discretePair holCat)
    (ProductObject discretePair [x, y])

holProductMorphism :: Morphism a -> Morphism a -> Morphism a
holProductMorphism x y =
  FMapMorphism
    (RightAdjoint $ LimitAdjunction discretePair holCat)
    (ProductMorphism discretePair [x, y])

holCoproductMorphism :: Morphism a -> Morphism a -> Morphism a
holCoproductMorphism x y =
  FMapMorphism
    (LeftAdjoint $ ColimitAdjunction discretePair holCat)
    (ProductMorphism discretePair [x, y])

holFixObj :: Functor' a -> Object a
holFixObj f = FMapObject (LeftAdjoint $ FreeForgetfulAlgebra f) holVoid

holFixMorphism :: Functor' a -> Morphism a -> Morphism a
holFixMorphism f = FMapMorphism (LeftAdjoint $ FreeForgetfulAlgebra f)

holCofixObj :: Functor' a -> Object a
holCofixObj f = FMapObject (RightAdjoint $ ForgetfulCofreeAlgebra f) holUnit

holCofixMorphism :: Functor' a -> Morphism a -> Morphism a
holCofixMorphism f = FMapMorphism (RightAdjoint $ ForgetfulCofreeAlgebra f)

holConstUnit :: Functor' a
holConstUnit = ConstFunctor holUnit

holIdentityFunctor :: Functor' a
holIdentityFunctor = IdentityFunctor holCat

holFunctorCat :: Category a
holFunctorCat = FunctorCat holCat holCat

holProductFunctor :: Functor' a -> Functor' a -> Functor' a
holProductFunctor f g =
  FunctorCatObject $
    FMapObject
      (RightAdjoint $ LimitAdjunction discretePair holFunctorCat)
      (ProductObject discretePair [FunctorObject f, FunctorObject g])

holCoproductFunctor :: Functor' a -> Functor' a -> Functor' a
holCoproductFunctor f g =
  FunctorCatObject $
    FMapObject
      (LeftAdjoint $ ColimitAdjunction discretePair holFunctorCat)
      (ProductObject discretePair [FunctorObject f, FunctorObject g])

holNatFunctor :: Functor' a
holNatFunctor = holCoproductFunctor holConstUnit holIdentityFunctor

holNat :: Object a
holNat = holFixObj holNatFunctor

holNatFold :: Morphism a -> Morphism a -> Morphism a
holNatFold z s = holFixMorphism holNatFunctor $ holCoproductMorphism z s

holInjectLeft :: Object a -> Object a -> Morphism a -> Morphism a
holInjectLeft x y f =
  ComposedMorphism
    (ProjectMorphism discretePair x holCat)
    [ AdjunctionUnit
        (ColimitAdjunction discretePair holCat)
        (ProductObject discretePair [x, y]),
      FMapMorphism (DiagonalFunctor discretePair holCat) f
    ]

holInjectRight :: Object a -> Object a -> Morphism a -> Morphism a
holInjectRight x y f =
  ComposedMorphism
    (ProjectMorphism discretePair y holCat)
    [ AdjunctionUnit
        (ColimitAdjunction discretePair holCat)
        (ProductObject discretePair [x, y]),
      FMapMorphism (DiagonalFunctor discretePair holCat) f
    ]
