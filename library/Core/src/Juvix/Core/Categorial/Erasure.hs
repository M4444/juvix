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
    MinimalInstanceAlgebra,
    Morphism (..),
    Object (..),
    Term (..),
    UnannotatedMorphism (..),
  )
import qualified Juvix.Core.Categorial.Private.Utils ()
import Juvix.Library
  ( Maybe (..),
    Monad (..),
    mapM,
    return,
    ($),
    (<$>),
  )

type EraseResultT m a freeAlgObj = ExceptT.ExceptT (EraseError freeAlgObj) m a

newtype EraseChecks m annotated erased = EraseChecks
  { eraseFunction ::
      annotated ->
      m erased
  }

eraseUnannotatedMorphism ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  UnannotatedMorphism annotated ->
  EraseResultT m (UnannotatedMorphism erased) annotated
eraseUnannotatedMorphism checks (FreeAlgMorphism morphism) = do
  erased <- Trans.lift $ eraseFunction checks morphism
  return $ FreeAlgMorphism erased
eraseUnannotatedMorphism checks (Composition morphisms) =
  Composition <$> mapM (eraseMorphism checks) morphisms

eraseMorphism ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  Morphism annotated ->
  EraseResultT m (Morphism erased) annotated
eraseMorphism checks (Morphism unannotated (Just _annotation)) = do
  erased <- eraseUnannotatedMorphism checks unannotated
  return $ Morphism erased Nothing
eraseMorphism _checks (Morphism unannotated Nothing) =
  ExceptT.throwE $ CategorialErrors.AlreadyErasedMorphism unannotated

eraseAbstract ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  AbstractTerm annotated ->
  EraseResultT m (AbstractTerm erased) annotated
eraseAbstract checks (MorphismTerm morphism) =
  MorphismTerm <$> eraseMorphism checks morphism
-- Any categorial term other than a MorphismTerm is a pure specification,
-- with no executable interpretation.  Consequently, erasing anything other
-- than a MorphismTerm leaves no information whatsoever.  We can allow
-- clients, for simplicity, to erase categorial terms that might be
-- non-functional (i.e. not morphisms), if we implement erasure of a
-- non-functional term as translation into a term that contains no information:
-- that is a terminal object.  We have available the higher terminal
-- category, which is a terminal object in a higher category.
eraseAbstract _checks _term = return $ ObjectTerm HigherTerminalObject

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
