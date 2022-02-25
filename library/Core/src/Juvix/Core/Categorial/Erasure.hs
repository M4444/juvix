module Juvix.Core.Categorial.Erasure
  ( erase,
    EraseChecks (..),
  )
where

import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import Juvix.Core.Categorial.Errors as CategorialErrors
  ( EraseError (..),
  )
import Juvix.Core.Categorial.Private.TermPrivate
  ( AbstractTerm (..),
    Category (..),
    Functor' (..),
    MinimalInstanceAlgebra,
    Morphism (..),
    Object (..),
    Term (..),
  )
import qualified Juvix.Core.Categorial.Private.Utils ()
import Juvix.Library
  ( Monad (..),
    return,
    void,
    ($),
    (<$>),
  )

type EraseResultT m a freeAlgObj = ExceptT.ExceptT (EraseError freeAlgObj) m a

newtype EraseChecks m annotated erased = EraseChecks
  { eraseFunction ::
      annotated ->
      m erased
  }

eraseCategory ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  Category annotated ->
  EraseResultT m (Category erased) annotated
eraseCategory _checks DirectedGraphCat = return DirectedGraphCat
eraseCategory _checks InitialCat = return InitialCat
eraseCategory _checks TerminalCat = return TerminalCat
eraseCategory _checks RefinedADTCat = return RefinedADTCat
eraseCategory _checks HigherOrderRefinedADTCat =
  return HigherOrderRefinedADTCat
eraseCategory checks (ProductCat cat cat') = do
  erased <- eraseCategory checks cat
  erased' <- eraseCategory checks cat'
  return $ ProductCat erased erased'
eraseCategory checks (OppositeCat cat) = do
  erased <- eraseCategory checks cat
  return $ OppositeCat erased
eraseCategory checks (SliceCat object) = do
  erased <- eraseObject checks object
  return $ SliceCat erased
eraseCategory checks (CosliceCat object) = do
  erased <- eraseObject checks object
  return $ CosliceCat erased
eraseCategory checks (FunctorCat cat cat') = do
  erased <- eraseCategory checks cat
  erased' <- eraseCategory checks cat'
  return $ FunctorCat erased erased'

eraseFunctor ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  Functor' annotated ->
  EraseResultT m (Functor' erased) annotated
eraseFunctor _checks functor =
  ExceptT.throwE $
    CategorialErrors.EraseUnimplemented
      (FunctorTerm functor)
      "Categorial.eraseFunctor"

eraseObject ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  Object annotated ->
  EraseResultT m (Object erased) annotated
eraseObject _checks object =
  ExceptT.throwE $
    CategorialErrors.EraseUnimplemented
      (ObjectTerm object)
      "Categorial.eraseObject"

eraseMorphism ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  Morphism annotated ->
  EraseResultT m (Morphism erased) annotated
eraseMorphism checks (IdentityMorphism object) = do
  void $ eraseObject checks object
  return ErasedIdentity
eraseMorphism checks (MorphismIntro _domain _codomain morphism) = do
  erased <- Trans.lift $ eraseFunction checks morphism
  return $ ErasedMorphism erased
eraseMorphism _checks ErasedIdentity =
  ExceptT.throwE $ CategorialErrors.AlreadyErasedMorphism ErasedIdentity
eraseMorphism _checks (ErasedMorphism morphism) =
  ExceptT.throwE $
    CategorialErrors.AlreadyErasedMorphism $ ErasedMorphism morphism
eraseMorphism _checks composed@(ErasedComposedMorphism _morphism _morphism') =
  ExceptT.throwE $ CategorialErrors.AlreadyErasedMorphism composed
eraseMorphism checks (ComposeMorphisms morphism morphism') = do
  erased <- eraseMorphism checks morphism
  erased' <- eraseMorphism checks morphism'
  return $ ErasedComposedMorphism erased erased'

eraseAbstract ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  AbstractTerm annotated ->
  EraseResultT m (AbstractTerm erased) annotated
eraseAbstract checks (CategoryTerm category) =
  CategoryTerm <$> eraseCategory checks category
eraseAbstract checks (FunctorTerm functor) = do
  erased <- eraseFunctor checks functor
  return $ FunctorTerm erased
eraseAbstract checks (ObjectTerm object) =
  ObjectTerm <$> eraseObject checks object
eraseAbstract checks (MorphismTerm morphism) =
  MorphismTerm <$> eraseMorphism checks morphism
eraseAbstract _checks term =
  ExceptT.throwE $
    CategorialErrors.EraseUnimplemented term "Categorial.checkAbstract"

erase ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  Term annotated ->
  EraseResultT m (Term erased) annotated
erase _eraseChecks term@(SexpRepresentation _) =
  ExceptT.throwE $ CategorialErrors.ErasingUncheckedTerm term
erase checks (RepresentedTerm abstract) = do
  erased <- eraseAbstract checks abstract
  return $ RepresentedTerm erased
