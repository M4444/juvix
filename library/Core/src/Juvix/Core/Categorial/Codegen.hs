module Juvix.Core.Categorial.Codegen
  ( CodegenFunctions (..),
    generateCode,
  )
where

import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import Juvix.Core.Categorial.Errors (CodegenError (..))
import Juvix.Core.Categorial.Private.TermPrivate
  ( AbstractTerm (..),
    MinimalInstanceAlgebra,
    Morphism (..),
    Object (..),
    Term (..),
  )
import Juvix.Library
  ( Maybe (..),
    Monad,
    mapM,
    ($),
  )

data CodegenFunctions m operation carrier = CodegenFunctions
  { genObj :: carrier -> m operation,
    genFunc :: Maybe (operation, operation) -> carrier -> m operation,
    genAtom :: operation -> m operation,
    genIdentity :: Maybe operation -> m operation,
    genCompose :: operation -> operation -> m operation
  }

type CodegenResultT m a carrier =
  ExceptT.ExceptT (CodegenError carrier) m a

generateObject ::
  Monad m =>
  CodegenFunctions m operation carrier ->
  Object carrier ->
  CodegenResultT m operation carrier
generateObject checks (CarrierObject object) =
  Trans.lift $ genObj checks object
generateObject _checks term@(FunctorApply _functor _object) =
  ExceptT.throwE $ CodegenUnimplemented (ObjectTerm term) "FunctorApply"
generateObject _checks term@(HigherObject _object) =
  ExceptT.throwE $ CodegenErased $ ObjectTerm term

generateMorphism ::
  Monad m =>
  CodegenFunctions m operation carrier ->
  Morphism carrier ->
  CodegenResultT m operation carrier
generateMorphism cf (CarrierMorphism signature morphism) =
  case signature of
    Just (domain, codomain) -> do
      domain' <- Trans.lift $ genObj cf domain
      codomain' <- Trans.lift $ genObj cf codomain
      Trans.lift $ genFunc cf (Just (domain', codomain')) morphism
    Nothing ->
      Trans.lift $ genFunc cf Nothing morphism
generateMorphism cf (IdentityMorphism obj) = do
  obj' <- mapM (generateObject cf) obj
  Trans.lift $ genIdentity cf obj'
generateMorphism cf (ComposedMorphism f []) =
  generateMorphism cf f
generateMorphism cf (ComposedMorphism f (g : gs)) = do
  f' <- generateMorphism cf f
  gs' <- generateMorphism cf (ComposedMorphism g gs)
  Trans.lift $ genCompose cf f' gs'
generateMorphism _cf morphism@(HigherMorphism _) =
  ExceptT.throwE $ CodegenErased $ MorphismTerm morphism

generateAbstract ::
  Monad m =>
  CodegenFunctions m operation carrier ->
  AbstractTerm carrier ->
  CodegenResultT m operation carrier
generateAbstract cf (MorphismTerm morphism) =
  generateMorphism cf morphism
-- Morphisms are the only terms from which code can be generated -- other
-- term types are pure specifications.
generateAbstract _cf term =
  ExceptT.throwE $ CodegenErased term

generateCode ::
  ( Monad m,
    MinimalInstanceAlgebra carrier
  ) =>
  CodegenFunctions m operation carrier ->
  Term carrier ->
  CodegenResultT m operation carrier
generateCode _cf (SexpRepresentation sexp) =
  ExceptT.throwE $ CodegenUnchecked sexp
generateCode cf (RepresentedTerm term) =
  generateAbstract cf term
