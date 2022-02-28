module Juvix.Core.Categorial.Codegen
  ( CodegenFunctions (..),
    generateCode,
  )
where

import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Juvix.Core.Categorial.Errors as CategorialErrors
import qualified Juvix.Core.Categorial.Private.TermPrivate as TermPrivate
import Juvix.Library
  ( Maybe (..),
    Monad,
    fst,
    map,
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
  ExceptT.ExceptT (CategorialErrors.CodegenError carrier) m a

generateMorphismUsingSignature ::
  Monad m =>
  CodegenFunctions m operation carrier ->
  Maybe (operation, operation) ->
  TermPrivate.UnannotatedMorphism carrier ->
  CodegenResultT m operation carrier
generateMorphismUsingSignature
  cf
  signature
  (TermPrivate.CarrierMorphism morphism) =
    Trans.lift $ genFunc cf signature morphism
generateMorphismUsingSignature
  cf
  signature
  (TermPrivate.Composition []) =
    Trans.lift $ genIdentity cf $ map fst signature
generateMorphismUsingSignature
  cf
  _signature
  (TermPrivate.Composition [morphism]) =
    generateMorphism cf morphism
generateMorphismUsingSignature
  cf
  _signature
  (TermPrivate.Composition (morphism : morphisms)) = do
    left <- generateMorphism cf morphism
    right <-
      generateMorphismUsingSignature cf Nothing (TermPrivate.Composition morphisms)
    Trans.lift $ genCompose cf left right

generateMorphismCommon ::
  Monad m =>
  CodegenFunctions m operation carrier ->
  TermPrivate.UnannotatedMorphism carrier ->
  Maybe (TermPrivate.Annotation carrier) ->
  CodegenResultT m operation carrier
generateMorphismCommon
  cf
  unannotated
  (Just (TermPrivate.Annotation domain codomain)) = do
    domain' <- Trans.lift $ genObj cf domain
    codomain' <- Trans.lift $ genObj cf codomain
    generateMorphismUsingSignature cf (Just (domain', codomain')) unannotated
generateMorphismCommon
  cf
  unannotated
  Nothing = generateMorphismUsingSignature cf Nothing unannotated

generateMorphism ::
  Monad m =>
  CodegenFunctions m operation carrier ->
  TermPrivate.Morphism carrier ->
  CodegenResultT m operation carrier
generateMorphism cf (TermPrivate.Morphism unannotated annotation) =
  generateMorphismCommon cf unannotated annotation

generateAbstract ::
  Monad m =>
  CodegenFunctions m operation carrier ->
  TermPrivate.AbstractTerm carrier ->
  CodegenResultT m operation carrier
generateAbstract cf (TermPrivate.MorphismTerm morphism) =
  generateMorphism cf morphism
-- Morphisms are the only terms from which code can be generated -- other
-- term types are pure specifications.
generateAbstract _cf term =
  ExceptT.throwE $ CategorialErrors.CodegenErased term

generateCode ::
  ( Monad m,
    TermPrivate.MinimalInstanceAlgebra carrier
  ) =>
  CodegenFunctions m operation carrier ->
  TermPrivate.Term carrier ->
  CodegenResultT m operation carrier
generateCode _cf (TermPrivate.SexpRepresentation sexp) =
  ExceptT.throwE $ CategorialErrors.CodegenUnchecked sexp
generateCode cf (TermPrivate.RepresentedTerm term) =
  generateAbstract cf term
