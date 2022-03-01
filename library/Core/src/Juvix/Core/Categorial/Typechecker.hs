module Juvix.Core.Categorial.Typechecker
  ( IntroChecks (..),
    checkIntro,
    ElimChecks (..),
    checkElim,
    AbstractChecks (..),
    Equiv,
  )
where

import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import Juvix.Core.Categorial.Errors
  ( CheckError (..),
  )
import Juvix.Core.Categorial.Private.TermPrivate
  ( AbstractTerm (..),
    Adjunction (..),
    Category (..),
    ConcreteTerm,
    Functor' (..),
    HigherCategory (..),
    Keyword (..),
    MinimalInstanceAlgebra,
    Morphism (..),
    Object (..),
    Symbol (..),
    Term (..),
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
decodeAlgebra term = ExceptT.throwE $ ExpectedAlgebraTerm term

decode ::
  ( Monad m,
    MinimalInstanceAlgebra carrier
  ) =>
  ConcreteTerm carrier ->
  CheckResultT m (AbstractTerm carrier) carrier
decode _term@(SexpTypes.Atom (SexpTypes.P (Keyword k) _)) =
  case k of
    KRefinedADTCat -> return $ CategoryTerm $ RefinedADTCat MinimalMetalogic
    _ -> ExceptT.throwE $ KeywordRequiresArguments k
decode term@(SexpTypes.Atom a) =
  ExceptT.throwE $ InvalidAtom term a
decode SexpTypes.Nil = ExceptT.throwE EmptySexp
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
            MorphismTerm (CarrierMorphism (Just (domain, codomain)) morphism)
      _ ->
        ExceptT.throwE $
          WrongNumberOfArgumentsForKeyword KCarrierMorphism
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
          WrongNumberOfArgumentsForKeyword KCarrierObject
decode term@(SexpTypes.Cons _ _) =
  ExceptT.throwE $ IllFormedSExpression term

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

checkCategory ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Category uncheckedCarrier ->
  CheckResultT
    m
    (HigherCategory checkedCarrier, Category checkedCarrier)
    uncheckedCarrier
checkCategory checks (DirectedGraphCat higher) = do
  (_, checked) <- checkHigherCategory checks higher
  return (checked, DirectedGraphCat checked)
checkCategory checks (InitialCat higher) = do
  (higher', cat') <- checkHigherCategory checks higher
  return (higher', InitialCat cat')
checkCategory checks (TerminalCat higher) = do
  (higher', cat') <- checkHigherCategory checks higher
  return (higher', TerminalCat cat')
checkCategory checks (RefinedADTCat higher) = do
  (higher', cat') <- checkHigherCategory checks higher
  return (higher', RefinedADTCat cat')
checkCategory checks (HigherOrderRefinedADTCat higher) = do
  (higher', cat') <- checkHigherCategory checks higher
  return (higher', HigherOrderRefinedADTCat cat')
checkCategory checks (ProductCat cat cat') = do
  (higher, checked) <- checkCategory checks cat
  (higher', checked') <- checkCategory checks cat'
  unless (equiv higher higher') $
    ExceptT.throwE $ HigherCategoryMismatch cat cat'
  return (higher, ProductCat checked checked')
checkCategory checks (OppositeCat cat) = do
  (higher, checked) <- checkCategory checks cat
  return (higher, OppositeCat checked)
checkCategory checks (SliceCat object) = do
  (higher, _cat, checked) <- checkObject checks object
  return (higher, SliceCat checked)
checkCategory checks (CosliceCat object) = do
  (higher, _cat, checked) <- checkObject checks object
  return (higher, CosliceCat checked)
checkCategory checks (FunctorCat cat cat') = do
  (higher, checked) <- checkCategory checks cat
  (higher', checked') <- checkCategory checks cat'
  unless (equiv higher higher') $
    ExceptT.throwE $ HigherCategoryMismatch cat cat'
  return (higher, FunctorCat checked checked')
checkCategory _checks term@(AdjunctionCat _adj) =
  ExceptT.throwE $
    CheckUnimplemented (CategoryTerm term) "checking adjunction category"

instance (Eq carrier) => Equiv (Category carrier) where
  equiv = (==)

checkObject ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Object uncheckedCarrier ->
  CheckResultT
    m
    ( HigherCategory checkedCarrier,
      Category checkedCarrier,
      Object checkedCarrier
    )
    uncheckedCarrier
checkObject checks (CarrierObject obj) = do
  checked <- checkVariableAsType checks obj
  return
    ( MinimalMetalogic,
      HigherOrderRefinedADTCat MinimalMetalogic,
      CarrierObject checked
    )
checkObject _checks term@(HigherObject _cat) =
  ExceptT.throwE $ CheckUnimplemented (ObjectTerm term) "checking higherObject"
checkObject _checks term@(FunctorApply _functor _object) =
  ExceptT.throwE $ CheckUnimplemented (ObjectTerm term) "checking functorApply"

instance (Eq carrier) => Equiv (Object carrier) where
  equiv = (==)

checkMorphism ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Morphism uncheckedCarrier ->
  CheckResultT
    m
    ( HigherCategory checkedCarrier,
      Category checkedCarrier,
      Object checkedCarrier,
      Object checkedCarrier,
      Morphism checkedCarrier
    )
    uncheckedCarrier
checkMorphism _checks morphism@(CarrierMorphism Nothing _function) =
  ExceptT.throwE $ CheckingErasedMorphism morphism
checkMorphism checks (CarrierMorphism (Just (domain, codomain)) function) = do
  domain <- checkVariableAsType checks domain
  codomain <- checkVariableAsType checks codomain
  function <- checkVariableAsFunction checks domain codomain function
  return
    ( MinimalMetalogic,
      HigherOrderRefinedADTCat MinimalMetalogic,
      CarrierObject domain,
      CarrierObject codomain,
      CarrierMorphism (Just (domain, codomain)) function
    )
checkMorphism _checks morphism@(IdentityMorphism Nothing) =
  ExceptT.throwE $ CheckingErasedMorphism morphism
checkMorphism checks (IdentityMorphism (Just object)) = do
  (_, _, checked) <- checkObject checks object
  return
    ( MinimalMetalogic,
      HigherOrderRefinedADTCat MinimalMetalogic,
      checked,
      checked,
      IdentityMorphism $ Just checked
    )
checkMorphism checks (ComposedMorphism f []) =
  checkMorphism checks f
checkMorphism checks (ComposedMorphism f (g : gs)) = do
  (_, _, fDom, fCod, f') <- checkMorphism checks f
  (_, _, gsDom, gsCod, gs') <- checkMorphism checks (ComposedMorphism g gs)
  unless (equiv fCod gsDom) $
    ExceptT.throwE $ IllegalMorphismComposition f (ComposedMorphism g gs)
  return
    ( MinimalMetalogic,
      HigherOrderRefinedADTCat MinimalMetalogic,
      fDom,
      gsCod,
      ComposedMorphism f' [gs']
    )
checkMorphism _checks term@(HigherMorphism _morphism) =
  ExceptT.throwE $
    CheckUnimplemented (MorphismTerm term) "checking HigherMorphism"

instance (Eq carrier) => Equiv (Morphism carrier) where
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
    ( HigherCategory checkedCarrier,
      Category checkedCarrier,
      Category checkedCarrier,
      Functor' checkedCarrier
    )
    uncheckedCarrier
checkFunctor checks (IdentityFunctor cat) = do
  (higher, checked) <- checkCategory checks cat
  return (higher, checked, checked, IdentityFunctor checked)
checkFunctor checks (DiagonalFunctor cat) = do
  (higher, checked) <- checkCategory checks cat
  return (higher, checked, ProductCat checked checked, DiagonalFunctor checked)
checkFunctor checks (ProductFunctor cat) = do
  (higher, checked) <- checkCategory checks cat
  return (higher, ProductCat checked checked, checked, ProductFunctor checked)
checkFunctor checks (CoproductFunctor cat) = do
  (higher, checked) <- checkCategory checks cat
  return (higher, ProductCat checked checked, checked, CoproductFunctor checked)
checkFunctor checks (ComposedFunctor f []) =
  checkFunctor checks f
checkFunctor checks (ComposedFunctor f (g : gs)) = do
  (higher, fDom, fCod, f') <-
    checkFunctor checks f
  (higher', gsDom, gsCod, gs') <-
    checkFunctor checks (ComposedFunctor g gs)
  unless (equiv higher higher') $
    ExceptT.throwE $ IllegalFunctorComposition f (ComposedFunctor g gs)
  unless (equiv fCod gsDom) $
    ExceptT.throwE $ IllegalFunctorComposition f (ComposedFunctor g gs)
  return (higher, fDom, gsCod, ComposedFunctor f' [gs'])
checkFunctor checks (LeftFunctor f) = do
  (higher, dom, cod, f') <- checkFunctor checks f
  case cod of
    ProductCat codLeft _codRight -> return (higher, dom, codLeft, f')
    _ -> ExceptT.throwE $ ProjectingNonProductFunctor f
checkFunctor checks (RightFunctor f) = do
  (higher, dom, cod, f') <- checkFunctor checks f
  case cod of
    ProductCat _codLeft codRight -> return (higher, dom, codRight, f')
    _ -> ExceptT.throwE $ ProjectingNonProductFunctor f
checkFunctor _checks term@(InitialFunctor _cat) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking InitialFunctor"
checkFunctor _checks term@(TerminalFunctor _cat) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking TerminalFunctor"
checkFunctor _checks term@(ConstFunctor _obj) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking ConstFunctor"
checkFunctor _checks term@(FreeFunctor _obj) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking FreeFunctor"
checkFunctor _checks term@(CofreeFunctor _obj) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking CofreeFunctor"
checkFunctor _checks term@(ForgetAlgebraFunctor _obj) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking ForgetAlgebraFunctor"
checkFunctor _checks term@(ForgetCoalgebraFunctor _obj) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking ForgetCoalgebraFunctor"
checkFunctor _checks term@(CurryFunctor _obj) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking CurryFunctor"
checkFunctor _checks term@(UncurryFunctor _obj) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking UncurryFunctor"
checkFunctor _checks term@(BaseChangeFunctor _x _y) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking BaseChangeFunctor"
checkFunctor _checks term@(CobaseChangeFunctor _x _y) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking CobaseChangeFunctor"

instance (Eq carrier) => Equiv (Functor' carrier) where
  equiv = (==)

-- | Returns the categories between which the adjoint functors map,
-- as well as the adjoint functors themselves.
checkAdjunction ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Adjunction uncheckedCarrier ->
  CheckResultT
    m
    ( HigherCategory checkedCarrier,
      Category checkedCarrier,
      Category checkedCarrier,
      Functor' checkedCarrier,
      Functor' checkedCarrier,
      Adjunction checkedCarrier
    )
    uncheckedCarrier
checkAdjunction _checks adj =
  ExceptT.throwE $ CheckUnimplemented (AdjunctionTerm adj) "adjunction checking"

instance (Eq carrier) => Equiv (Adjunction carrier) where
  equiv = (==)

-- | Returns the higher category in terms over which the given higher
-- category is enriched, as well as the checked higher category itself.
checkHigherCategory ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  HigherCategory uncheckedCarrier ->
  CheckResultT
    m
    (HigherCategory checkedCarrier, HigherCategory checkedCarrier)
    uncheckedCarrier
checkHigherCategory _checks MinimalMetalogic =
  return (MinimalMetalogic, MinimalMetalogic)

instance (Eq carrier) => Equiv (HigherCategory carrier) where
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
checkAbstract checks (ObjectTerm object) = do
  (_, _, checked) <- checkObject checks object
  return $ ObjectTerm checked
checkAbstract checks (MorphismTerm morphism) = do
  (_, _, _, _, checked) <- checkMorphism checks morphism
  return $ MorphismTerm checked
checkAbstract checks (CategoryTerm category) = do
  (_, checked) <- checkCategory checks category
  return $ CategoryTerm checked
checkAbstract checks (FunctorTerm functor) = do
  (_, _, _, checked) <- checkFunctor checks functor
  return $ FunctorTerm checked
checkAbstract checks (AdjunctionTerm adj) = do
  (_, _, _, _, _, checked) <- checkAdjunction checks adj
  return $ AdjunctionTerm checked
checkAbstract checks (HigherCategoryTerm category) = do
  (_, checked) <- checkHigherCategory checks category
  return $ HigherCategoryTerm checked

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
  ExceptT.throwE $ AlreadyCheckedTerm abstract

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
        CheckUnimplemented term "object elimination"
    MorphismTerm _m ->
      ExceptT.throwE $
        CheckUnimplemented term "morphism elimination"
    _ ->
      ExceptT.throwE $ NonEliminatableTerm term

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
  ExceptT.throwE $ AlreadyCheckedTerm abstract
