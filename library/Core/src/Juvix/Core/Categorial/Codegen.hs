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

data CodegenFunctions m operation freeAlgObj = CodegenFunctions
  { genObj :: freeAlgObj -> m operation,
    genFunc :: Maybe (operation, operation) -> freeAlgObj -> m operation,
    genAtom :: operation -> m operation,
    genIdentity :: Maybe operation -> m operation
  }

type CodegenResultT m a freeAlgObj =
  ExceptT.ExceptT (CategorialErrors.CodegenError freeAlgObj) m a

generateMorphismUsingSignature ::
  Monad m =>
  CodegenFunctions m operation freeAlgObj ->
  Maybe (operation, operation) ->
  TermPrivate.UnannotatedMorphism freeAlgObj ->
  CodegenResultT m operation freeAlgObj
generateMorphismUsingSignature cf signature (TermPrivate.Composition []) =
  Trans.lift $ genIdentity cf $ map fst signature
generateMorphismUsingSignature
  cf
  signature
  (TermPrivate.FreeAlgMorphism morphism) =
    Trans.lift $ genFunc cf signature morphism
generateMorphismUsingSignature _cf _signature term =
  ExceptT.throwE $
    CategorialErrors.CodegenUnimplemented
      (TermPrivate.MorphismTerm (TermPrivate.Morphism term Nothing))
      "generateMorphismUsingSignature"

generateMorphismCommon ::
  Monad m =>
  CodegenFunctions m operation freeAlgObj ->
  TermPrivate.UnannotatedMorphism freeAlgObj ->
  Maybe (TermPrivate.Annotation freeAlgObj) ->
  CodegenResultT m operation freeAlgObj
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
  CodegenFunctions m operation freeAlgObj ->
  TermPrivate.Morphism freeAlgObj ->
  CodegenResultT m operation freeAlgObj
generateMorphism cf (TermPrivate.Morphism unannotated annotation) =
  generateMorphismCommon cf unannotated annotation

generateAbstract ::
  Monad m =>
  CodegenFunctions m operation freeAlgObj ->
  TermPrivate.AbstractTerm freeAlgObj ->
  CodegenResultT m operation freeAlgObj
generateAbstract _cf term@(TermPrivate.CategoryTerm _cat) =
  ExceptT.throwE $ CategorialErrors.CodegenErased term
generateAbstract cf (TermPrivate.MorphismTerm morphism) =
  generateMorphism cf morphism
generateAbstract _cf term =
  ExceptT.throwE $ CategorialErrors.CodegenUnimplemented term "generateAbstract"

generateCode ::
  ( Monad m,
    TermPrivate.MinimalInstanceAlgebra freeAlgObj
  ) =>
  CodegenFunctions m operation freeAlgObj ->
  TermPrivate.Term freeAlgObj ->
  CodegenResultT m operation freeAlgObj
generateCode _cf (TermPrivate.SexpRepresentation sexp) =
  ExceptT.throwE $ CategorialErrors.CodegenUnchecked sexp
generateCode cf (TermPrivate.RepresentedTerm term) =
  generateAbstract cf term
