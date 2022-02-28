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
    void,
    ($),
    (.),
    (<$>),
    (<&>),
    (==),
  )
import qualified Juvix.Sexp.Types as SexpTypes

type CheckResultT m a carrier = ExceptT.ExceptT (CheckError carrier) m a

decodeAlgebra ::
  ( Monad m,
    MinimalInstanceAlgebra carrier
  ) =>
  ConcreteTerm carrier ->
  CheckResultT m carrier carrier
decodeAlgebra (SexpTypes.Atom (SexpTypes.P (Variable v) _)) =
  return v
decodeAlgebra term = ExceptT.throwE $ CategorialErrors.ExpectedAlgebraTerm term

decode ::
  ( Monad m,
    MinimalInstanceAlgebra carrier
  ) =>
  ConcreteTerm carrier ->
  CheckResultT m (AbstractTerm carrier) carrier
decode _term@(SexpTypes.Atom (SexpTypes.P (Keyword k) _)) =
  case k of
    KRefinedADTCat -> return $ CategoryTerm RefinedADTCat
    _ -> ExceptT.throwE $ CategorialErrors.KeywordRequiresArguments k
decode term@(SexpTypes.Atom a) =
  ExceptT.throwE $ CategorialErrors.InvalidAtom term a
decode SexpTypes.Nil = ExceptT.throwE CategorialErrors.EmptySexp
decode
  ( SexpTypes.Cons
      (SexpTypes.Atom (SexpTypes.P (Keyword KCarrierMorphism) _))
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
              Morphism (CarrierMorphism morphism) (Just (Annotation domain codomain))
      _ ->
        ExceptT.throwE $
          CategorialErrors.WrongNumberOfArgumentsForKeyword KCarrierMorphism
decode
  ( SexpTypes.Cons
      (SexpTypes.Atom (SexpTypes.P (Keyword KCarrierObject) _))
      sexp
    ) =
    case sexp of
      SexpTypes.Cons object SexpTypes.Nil -> do
        object <- decodeAlgebra object
        return $ ObjectTerm $ CarrierObject object
      _ ->
        ExceptT.throwE $
          CategorialErrors.WrongNumberOfArgumentsForKeyword KCarrierObject
decode term@(SexpTypes.Cons _ _) =
  ExceptT.throwE $ CategorialErrors.IllFormedSExpression term

class Equiv a where
  equiv :: a -> a -> Bool

checkVariableAsType ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  uncheckedCarrier ->
  CheckResultT m checkedCarrier uncheckedCarrier
checkVariableAsType checks var = Trans.lift $ checkAsType checks var

checkVariableAsFunction ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  checkedCarrier ->
  checkedCarrier ->
  uncheckedCarrier ->
  CheckResultT m checkedCarrier uncheckedCarrier
checkVariableAsFunction checks domain codomain var =
  Trans.lift $ checkAsFunction checks domain codomain var

checkObject ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Object uncheckedCarrier ->
  CheckResultT m (Object checkedCarrier) uncheckedCarrier
checkObject checks (CarrierObject obj) =
  Trans.lift $ CarrierObject <$> checkAsType checks obj
checkObject checks (HigherObject cat) =
  HigherObject <$> checkCategory checks cat

instance (Eq carrier) => Equiv (Object carrier) where
  equiv = (==)

checkMorphismWithSignature ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  uncheckedCarrier ->
  uncheckedCarrier ->
  UnannotatedMorphism uncheckedCarrier ->
  CheckResultT m (Morphism checkedCarrier) uncheckedCarrier
checkMorphismWithSignature checks domain codomain (CarrierMorphism function) = do
  domain <- checkVariableAsType checks domain
  codomain <- checkVariableAsType checks codomain
  function <- checkVariableAsFunction checks domain codomain function
  return $ Morphism (CarrierMorphism function) $ Just (Annotation domain codomain)
checkMorphismWithSignature checks domain codomain (Composition []) = do
  unless (domain == codomain) $
    ExceptT.throwE $
      CategorialErrors.IdentityBetweenDifferentObjects domain codomain
  domain' <- checkVariableAsType checks domain
  return $ Morphism (Composition []) $ Just (Annotation domain' domain')
checkMorphismWithSignature
  checks
  _domain
  _codomain
  (Composition [morphism]) =
    checkMorphism checks morphism
checkMorphismWithSignature
  checks
  domain
  codomain
  ( Composition
      (Morphism morphism (Just (Annotation domain' codomain')) : morphisms)
    ) = do
    left <-
      checkMorphism checks $
        Morphism morphism (Just $ Annotation domain' codomain')
    checkedDomain <- checkVariableAsType checks domain
    checkedDomain' <- checkVariableAsType checks domain'
    checkedCodomain <- checkVariableAsType checks codomain
    unless (checkedDomain == checkedDomain') $
      ExceptT.throwE $
        CategorialErrors.IllTypedMorphismComposition morphism domain domain'
    void $ checkVariableAsType checks codomain'
    right <-
      checkMorphismWithSignature
        checks
        codomain'
        codomain
        (Composition morphisms)
    return $
      Morphism (Composition [left, right]) $
        Just (Annotation checkedDomain checkedCodomain)
checkMorphismWithSignature
  _checks
  _domain
  _codomain
  (Composition (Morphism morphism Nothing : _morphims)) =
    ExceptT.throwE $ CategorialErrors.CheckingMorphismAfterErasure morphism

checkMorphism ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Morphism uncheckedCarrier ->
  CheckResultT m (Morphism checkedCarrier) uncheckedCarrier
checkMorphism checks (Morphism morphism (Just (Annotation domain codomain))) =
  checkMorphismWithSignature checks domain codomain morphism
checkMorphism _checks (Morphism morphism Nothing) =
  ExceptT.throwE $ CategorialErrors.CheckingMorphismAfterErasure morphism

checkCategory ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Category uncheckedCarrier ->
  CheckResultT m (Category checkedCarrier) uncheckedCarrier
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

instance (Eq carrier) => Equiv (Category carrier) where
  equiv = (==)

-- | Returns the functor signature (its domain and codomain categories)
-- | along with the checked version of the functor.
checkFunctor ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Functor' uncheckedCarrier ->
  CheckResultT
    m
    (Category checkedCarrier, Category checkedCarrier, Functor' checkedCarrier)
    uncheckedCarrier
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
checkFunctor checks (LeftFunctor f) = do
  (dom, cod, f') <- checkFunctor checks f
  case cod of
    ProductCat codLeft _codRight -> return (dom, codLeft, f')
    _ -> ExceptT.throwE $ CategorialErrors.ProjectingNonProductFunctor f
checkFunctor checks (RightFunctor f) = do
  (dom, cod, f') <- checkFunctor checks f
  case cod of
    ProductCat _codLeft codRight -> return (dom, codRight, f')
    _ -> ExceptT.throwE $ CategorialErrors.ProjectingNonProductFunctor f
checkFunctor _checks functor =
  ExceptT.throwE $
    CategorialErrors.CheckUnimplemented
      (FunctorTerm functor)
      "Categorial.checkFunctor"

instance (Eq carrier) => Equiv (Functor' carrier) where
  equiv = (==)

data AbstractChecks m uncheckedCarrier checkedCarrier = AbstractChecks
  { checkAsType ::
      uncheckedCarrier ->
      m checkedCarrier,
    checkAsFunction ::
      checkedCarrier ->
      checkedCarrier ->
      uncheckedCarrier ->
      m checkedCarrier
  }

checkAbstract ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  AbstractTerm uncheckedCarrier ->
  CheckResultT m (AbstractTerm checkedCarrier) uncheckedCarrier
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

newtype IntroChecks m uncheckedCarrier checkedCarrier = IntroChecks
  { introAbstractChecks :: AbstractChecks m uncheckedCarrier checkedCarrier
  }

checkIntroAbstract ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  IntroChecks m uncheckedCarrier checkedCarrier ->
  AbstractTerm uncheckedCarrier ->
  CheckResultT m (AbstractTerm checkedCarrier) uncheckedCarrier
checkIntroAbstract = checkAbstract . introAbstractChecks

checkIntro ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  IntroChecks m uncheckedCarrier checkedCarrier ->
  Term uncheckedCarrier ->
  CheckResultT m (Term checkedCarrier) uncheckedCarrier
checkIntro checks (SexpRepresentation sexp) =
  (decode sexp >>= checkIntroAbstract checks) <&> RepresentedTerm
checkIntro _checks (RepresentedTerm abstract) =
  ExceptT.throwE $ CategorialErrors.AlreadyCheckedTerm abstract

newtype ElimChecks m uncheckedCarrier checkedCarrier resultType = ElimChecks
  { elimAbstractChecks :: AbstractChecks m uncheckedCarrier checkedCarrier
  }

checkElimAbstract ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier,
    MinimalInstanceAlgebra resultType
  ) =>
  ElimChecks m uncheckedCarrier checkedCarrier resultType ->
  AbstractTerm uncheckedCarrier ->
  CheckResultT m resultType uncheckedCarrier
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
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier,
    MinimalInstanceAlgebra resultType
  ) =>
  ElimChecks m uncheckedCarrier checkedCarrier resultType ->
  Term uncheckedCarrier ->
  CheckResultT m resultType uncheckedCarrier
checkElim checks (SexpRepresentation sexp) =
  decode sexp >>= checkElimAbstract checks
checkElim _checks (RepresentedTerm abstract) =
  ExceptT.throwE $ CategorialErrors.AlreadyCheckedTerm abstract
