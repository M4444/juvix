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
  ( Monad,
    ($),
  )

data CodegenFunctions m operation freeAlgObj = CodegenFunctions
  { genObj :: freeAlgObj -> m operation,
    genFunc :: operation -> operation -> freeAlgObj -> m operation,
    genFuncErased :: freeAlgObj -> m operation,
    genAtom :: operation -> m operation,
    genIdentity :: operation -> m operation
  }

type CodegenResultT m a freeAlgObj =
  ExceptT.ExceptT (CategorialErrors.CodegenError freeAlgObj) m a

generateObject ::
  Monad m =>
  CodegenFunctions m operation freeAlgObj ->
  TermPrivate.Object freeAlgObj ->
  CodegenResultT m operation freeAlgObj
generateObject cf (TermPrivate.AlgebraObject obj) = Trans.lift $ genObj cf obj
generateObject _cf term@TermPrivate.HigherTerminalObject =
  ExceptT.throwE $ CategorialErrors.CodegenErased $ TermPrivate.ObjectTerm term

generateMorphism ::
  Monad m =>
  CodegenFunctions m operation freeAlgObj ->
  TermPrivate.Morphism freeAlgObj ->
  CodegenResultT m operation freeAlgObj
generateMorphism cf (TermPrivate.IdentityMorphism obj) = do
  objOp <- generateObject cf obj
  Trans.lift $ genIdentity cf objOp
generateMorphism cf (TermPrivate.MorphismIntro domain codomain morphism) = do
  domain <- Trans.lift $ genObj cf domain
  codomain <- Trans.lift $ genObj cf codomain
  Trans.lift $ genFunc cf domain codomain morphism
generateMorphism cf (TermPrivate.ErasedMorphism morphism) =
  Trans.lift $ genFuncErased cf morphism
generateMorphism _cf term =
  ExceptT.throwE $ CategorialErrors.CodegenErased $ TermPrivate.MorphismTerm term

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
