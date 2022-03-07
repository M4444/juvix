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
