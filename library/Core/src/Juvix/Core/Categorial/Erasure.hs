module Juvix.Core.Categorial.Erasure
  ( erase,
    EraseChecks (..),
  )
where

import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import Juvix.Core.Categorial.Errors
  ( EraseError (..),
  )
import Juvix.Core.Categorial.Private.TermPrivate
  ( AbstractTerm (..),
    Category (..),
    HigherCategory (..),
    MinimalInstanceAlgebra,
    Morphism (..),
    Shape (..),
    Term (..),
  )
import qualified Juvix.Core.Categorial.Private.Utils ()
import Juvix.Library
  ( Maybe (..),
    Monad (..),
    mapM,
    return,
    ($),
    (<$>),
    (<&>),
  )

type EraseResultT m a carrier = ExceptT.ExceptT (EraseError carrier) m a

newtype EraseChecks m annotated erased = EraseChecks
  { eraseFunction ::
      annotated ->
      m erased
  }

eraseMorphism ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  Morphism annotated ->
  EraseResultT m (Morphism erased) annotated
eraseMorphism checks (CarrierMorphism _signature morphism) =
  Trans.lift (eraseFunction checks morphism) <&> CarrierMorphism Nothing
eraseMorphism _checks (IdentityMorphism _object) =
  return $ IdentityMorphism Nothing
eraseMorphism checks (ComposedMorphism morphism morphisms) = do
  morphism' <- eraseMorphism checks morphism
  morphisms' <- mapM (eraseMorphism checks) morphisms
  return $ ComposedMorphism morphism' morphisms'
eraseMorphism _checks term@(FMapMorphism _functor _morphism) =
  ExceptT.throwE $ EraseUnimplemented (MorphismTerm term) "FMapMorphism"
eraseMorphism _checks term@(HigherMorphism _functor) =
  ExceptT.throwE $ AlreadyErasedMorphism term

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
-- non-functional term as translation into a term that contains no information.
-- One such term is an empty category.
eraseAbstract _checks _term =
  return $ CategoryTerm $ IndexCat MinimalMetalogic EmptyIndex

erase ::
  ( Monad m,
    MinimalInstanceAlgebra annotated,
    MinimalInstanceAlgebra erased
  ) =>
  EraseChecks m annotated erased ->
  Term annotated ->
  EraseResultT m (Term erased) annotated
erase _checks term@(SexpRepresentation _) =
  ExceptT.throwE $ ErasingUncheckedTerm term
erase checks (RepresentedTerm abstract) =
  RepresentedTerm <$> eraseAbstract checks abstract
