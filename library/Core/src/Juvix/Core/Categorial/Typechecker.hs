module Juvix.Core.Categorial.Typechecker
  ( IntroChecks (..),
    checkIntro,
    ElimChecks (..),
    checkElim,
    AbstractChecks (..),
  )
where

import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import Juvix.Core.Categorial.Errors as CategorialErrors
  ( CheckError (..),
  )
import Juvix.Core.Categorial.Private.TermPrivate
  ( AbstractTerm (..),
    Annotation (..),
    Category (..),
    ConcreteTerm,
    Functor' (..),
    Keyword (..),
    MinimalInstanceAlgebra,
    Morphism (..),
    Object (..),
    Symbol (..),
    Term (..),
    UnannotatedMorphism (..),
  )
import Juvix.Core.Categorial.Private.Theory ()
import qualified Juvix.Core.Categorial.Private.Utils ()
import Juvix.Library
  ( Bool (..),
    Eq,
    Maybe (..),
    Monad (..),
    return,
    unless,
    ($),
    (.),
    (<$>),
    (<&>),
    (==),
  )
import qualified Juvix.Sexp.Types as SexpTypes

type CheckResultT m a freeAlgObj = ExceptT.ExceptT (CheckError freeAlgObj) m a

decodeAlgebra ::
  ( Monad m,
    MinimalInstanceAlgebra freeAlgObj
  ) =>
  ConcreteTerm freeAlgObj ->
  CheckResultT m freeAlgObj freeAlgObj
decodeAlgebra (SexpTypes.Atom (SexpTypes.P (Variable v) _)) =
  return v
decodeAlgebra term = ExceptT.throwE $ CategorialErrors.ExpectedAlgebraTerm term

decode ::
  ( Monad m,
    MinimalInstanceAlgebra freeAlgObj
  ) =>
  ConcreteTerm freeAlgObj ->
  CheckResultT m (AbstractTerm freeAlgObj) freeAlgObj
decode (SexpTypes.Atom (SexpTypes.P (Variable a) _)) =
  return $ VariableTerm a
decode _term@(SexpTypes.Atom (SexpTypes.P (Keyword k) _)) =
  case k of
    KRefinedADTCat -> return $ CategoryTerm RefinedADTCat
    _ -> ExceptT.throwE $ CategorialErrors.KeywordRequiresArguments k
decode term@(SexpTypes.Atom a) =
  ExceptT.throwE $ CategorialErrors.InvalidAtom term a
decode SexpTypes.Nil = ExceptT.throwE CategorialErrors.EmptySexp
decode
  ( SexpTypes.Cons
      (SexpTypes.Atom (SexpTypes.P (Keyword KFreeAlgMorphism) _))
      sexp
    ) =
    case sexp of
      SexpTypes.Cons
        domain
        ( SexpTypes.Cons
            codomain
            (SexpTypes.Cons morphism SexpTypes.Nil)
          ) -> do
          domain <- decodeAlgebra domain
          codomain <- decodeAlgebra codomain
          morphism <- decodeAlgebra morphism
          return $
            MorphismTerm $
              Morphism (FreeAlgMorphism morphism) (Just (Annotation domain codomain))
      _ ->
        ExceptT.throwE $
          CategorialErrors.WrongNumberOfArgumentsForKeyword KFreeAlgMorphism
decode
  ( SexpTypes.Cons
      (SexpTypes.Atom (SexpTypes.P (Keyword KAlgObject) _))
      sexp
    ) =
    case sexp of
      SexpTypes.Cons object SexpTypes.Nil -> do
        object <- decodeAlgebra object
        return $ ObjectTerm $ AlgebraObject object
      _ ->
        ExceptT.throwE $
          CategorialErrors.WrongNumberOfArgumentsForKeyword KAlgObject
decode term@(SexpTypes.Cons _ _) =
  ExceptT.throwE $
    CategorialErrors.DecodeUnimplemented term "Categorial.decode cons"

class Equiv a where
  equiv :: a -> a -> Bool

checkVariableAsType ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  AbstractChecks m uncheckedAlg checkedAlg ->
  uncheckedAlg ->
  CheckResultT m checkedAlg uncheckedAlg
checkVariableAsType checks var = Trans.lift $ checkAsType checks var

checkVariableAsFunction ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  AbstractChecks m uncheckedAlg checkedAlg ->
  checkedAlg ->
  checkedAlg ->
  uncheckedAlg ->
  CheckResultT m checkedAlg uncheckedAlg
checkVariableAsFunction checks domain codomain var =
  Trans.lift $ checkAsFunction checks domain codomain var

checkObject ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  AbstractChecks m uncheckedAlg checkedAlg ->
  Object uncheckedAlg ->
  CheckResultT m (Object checkedAlg) uncheckedAlg
checkObject checks (AlgebraObject obj) = do
  checked <- Trans.lift $ checkAsType checks obj
  return $ AlgebraObject checked
checkObject _checks object =
  ExceptT.throwE $
    CategorialErrors.CheckUnimplemented
      (ObjectTerm object)
      "checkObject"

checkMorphismWithSignature ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  AbstractChecks m uncheckedAlg checkedAlg ->
  uncheckedAlg ->
  uncheckedAlg ->
  UnannotatedMorphism uncheckedAlg ->
  CheckResultT m (Morphism checkedAlg) uncheckedAlg
checkMorphismWithSignature checks domain codomain (FreeAlgMorphism function) = do
  domain <- checkVariableAsType checks domain
  codomain <- checkVariableAsType checks codomain
  function <- checkVariableAsFunction checks domain codomain function
  return $ Morphism (FreeAlgMorphism function) $ Just (Annotation domain codomain)
checkMorphismWithSignature _checks domain codomain morphism =
  ExceptT.throwE $
    CategorialErrors.CheckUnimplemented
      (MorphismTerm (Morphism morphism (Just (Annotation domain codomain))))
      "checkMorphism"

checkMorphism ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  AbstractChecks m uncheckedAlg checkedAlg ->
  Morphism uncheckedAlg ->
  CheckResultT m (Morphism checkedAlg) uncheckedAlg
checkMorphism checks (Morphism morphism (Just (Annotation domain codomain))) =
  checkMorphismWithSignature checks domain codomain morphism
checkMorphism _checks (Morphism morphism Nothing) =
  ExceptT.throwE $ CategorialErrors.CheckingMorphismAfterErasure morphism

checkCategory ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  AbstractChecks m uncheckedAlg checkedAlg ->
  Category uncheckedAlg ->
  CheckResultT m (Category checkedAlg) uncheckedAlg
checkCategory _checks DirectedGraphCat = return DirectedGraphCat
checkCategory _checks InitialCat = return InitialCat
checkCategory _checks TerminalCat = return TerminalCat
checkCategory _checks RefinedADTCat = return RefinedADTCat
checkCategory _checks HigherOrderRefinedADTCat =
  return HigherOrderRefinedADTCat
checkCategory checks (ProductCat cat cat') = do
  checked <- checkCategory checks cat
  checked' <- checkCategory checks cat'
  return $ ProductCat checked checked'
checkCategory checks (OppositeCat cat) = do
  checked <- checkCategory checks cat
  return $ OppositeCat checked
checkCategory checks (SliceCat object) = do
  checked <- checkObject checks object
  return $ SliceCat checked
checkCategory checks (CosliceCat object) = do
  checked <- checkObject checks object
  return $ CosliceCat checked
checkCategory checks (FunctorCat cat cat') = do
  checked <- checkCategory checks cat
  checked' <- checkCategory checks cat'
  return $ FunctorCat checked checked'

instance (Eq freeAlgObj) => Equiv (Category freeAlgObj) where
  equiv = (==)

-- | Returns the functor signature (its domain and codomain categories)
-- | along with the checked version of the functor.
checkFunctor ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  AbstractChecks m uncheckedAlg checkedAlg ->
  Functor' uncheckedAlg ->
  CheckResultT
    m
    (Category checkedAlg, Category checkedAlg, Functor' checkedAlg)
    uncheckedAlg
checkFunctor checks (IdentityFunctor cat) = do
  checked <- checkCategory checks cat
  return (checked, checked, IdentityFunctor checked)
checkFunctor checks (DiagonalFunctor cat) = do
  checked <- checkCategory checks cat
  return (checked, ProductCat checked checked, DiagonalFunctor checked)
checkFunctor checks (ProductFunctor cat) = do
  checked <- checkCategory checks cat
  return (ProductCat checked checked, checked, ProductFunctor checked)
checkFunctor checks (CoproductFunctor cat) = do
  checked <- checkCategory checks cat
  return (ProductCat checked checked, checked, CoproductFunctor checked)
checkFunctor checks (ComposeFunctors g f) = do
  (gDom, gCod, g') <- checkFunctor checks g
  (fDom, fCod, f') <- checkFunctor checks f
  unless (equiv fCod gDom) $
    ExceptT.throwE $ CategorialErrors.IllegalFunctorComposition g f
  return (fDom, gCod, ComposeFunctors g' f')
checkFunctor _checks functor =
  ExceptT.throwE $
    CategorialErrors.CheckUnimplemented
      (FunctorTerm functor)
      "Categorial.checkFunctor"

instance (Eq freeAlgObj) => Equiv (Functor' freeAlgObj) where
  equiv = (==)

data AbstractChecks m uncheckedAlg checkedAlg = AbstractChecks
  { checkAsType ::
      uncheckedAlg ->
      m checkedAlg,
    checkAsFunction ::
      checkedAlg ->
      checkedAlg ->
      uncheckedAlg ->
      m checkedAlg
  }

checkAbstract ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  AbstractChecks m uncheckedAlg checkedAlg ->
  AbstractTerm uncheckedAlg ->
  CheckResultT m (AbstractTerm checkedAlg) uncheckedAlg
checkAbstract checks (CategoryTerm category) =
  CategoryTerm <$> checkCategory checks category
checkAbstract checks (FunctorTerm functor) = do
  (_, _, checked) <- checkFunctor checks functor
  return $ FunctorTerm checked
checkAbstract checks (ObjectTerm object) =
  ObjectTerm <$> checkObject checks object
checkAbstract checks (MorphismTerm morphism) =
  MorphismTerm <$> checkMorphism checks morphism
checkAbstract _checks term =
  ExceptT.throwE $
    CategorialErrors.CheckUnimplemented term "Categorial.checkAbstract"

newtype IntroChecks m uncheckedAlg checkedAlg = IntroChecks
  { introAbstractChecks :: AbstractChecks m uncheckedAlg checkedAlg
  }

checkIntroAbstract ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  IntroChecks m uncheckedAlg checkedAlg ->
  AbstractTerm uncheckedAlg ->
  CheckResultT m (AbstractTerm checkedAlg) uncheckedAlg
checkIntroAbstract = checkAbstract . introAbstractChecks

checkIntro ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg
  ) =>
  IntroChecks m uncheckedAlg checkedAlg ->
  Term uncheckedAlg ->
  CheckResultT m (Term checkedAlg) uncheckedAlg
checkIntro checks (SexpRepresentation sexp) =
  (decode sexp >>= checkIntroAbstract checks) <&> RepresentedTerm
checkIntro _checks (RepresentedTerm abstract) =
  ExceptT.throwE $ CategorialErrors.AlreadyCheckedTerm abstract

newtype ElimChecks m uncheckedAlg checkedAlg resultType = ElimChecks
  { elimAbstractChecks :: AbstractChecks m uncheckedAlg checkedAlg
  }

checkElimAbstract ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg,
    MinimalInstanceAlgebra resultType
  ) =>
  ElimChecks m uncheckedAlg checkedAlg resultType ->
  AbstractTerm uncheckedAlg ->
  CheckResultT m resultType uncheckedAlg
checkElimAbstract checks term = do
  checked <- checkAbstract (elimAbstractChecks checks) term
  case checked of
    ObjectTerm _o ->
      ExceptT.throwE $
        CategorialErrors.CheckUnimplemented term "object elimination"
    MorphismTerm _m ->
      ExceptT.throwE $
        CategorialErrors.CheckUnimplemented term "morphism elimination"
    _ ->
      ExceptT.throwE $ CategorialErrors.NonEliminatableTerm term

checkElim ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedAlg,
    MinimalInstanceAlgebra checkedAlg,
    MinimalInstanceAlgebra resultType
  ) =>
  ElimChecks m uncheckedAlg checkedAlg resultType ->
  Term uncheckedAlg ->
  CheckResultT m resultType uncheckedAlg
checkElim checks (SexpRepresentation sexp) =
  decode sexp >>= checkElimAbstract checks
checkElim _checks (RepresentedTerm abstract) =
  ExceptT.throwE $ CategorialErrors.AlreadyCheckedTerm abstract
