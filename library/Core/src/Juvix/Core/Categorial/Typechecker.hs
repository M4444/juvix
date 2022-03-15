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
import Juvix.Core.Categorial.Private.TermLibrary ()
import Juvix.Core.Categorial.Private.TermPrivate
  ( AbstractTerm (..),
    Adjunction (..),
    Category (..),
    ConcreteTerm,
    Diagram,
    Functor' (..),
    HigherCategory (..),
    Keyword (..),
    MinimalInstanceAlgebra,
    Morphism (..),
    Object (..),
    Shape (..),
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
    pure,
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

class Normalize m a where
  normalize :: (Monad m, MinimalInstanceAlgebra a) => a -> m a

class Equiv m a where
  equiv :: (Monad m, MinimalInstanceAlgebra a) => a -> a -> m Bool

normalizedEq ::
  ( Monad m,
    MinimalInstanceAlgebra a,
    Normalize m a
  ) =>
  a ->
  a ->
  m Bool
normalizedEq x x' = do
  n <- normalize x
  n' <- normalize x'
  return $ n == n'

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
checkCategory checks (RefinedADTCat higher) = do
  (higher', cat') <- checkHigherCategory checks higher
  return (higher', RefinedADTCat cat')
checkCategory checks (HigherOrderRefinedADTCat higher) = do
  (higher', cat') <- checkHigherCategory checks higher
  return (higher', HigherOrderRefinedADTCat cat')
checkCategory checks (IndexCat higher shape) = do
  (_, higher') <- checkHigherCategory checks higher
  shape' <- checkShape checks shape
  return (higher', IndexCat higher' shape')
checkCategory checks (OppositeCat cat) = do
  (higher, checked) <- checkCategory checks cat
  return (higher, OppositeCat checked)
checkCategory checks (SliceCat object) = do
  (higher, _cat, checked) <- checkObject checks object
  return (higher, SliceCat checked)
checkCategory checks (CosliceCat object) = do
  (higher, _cat, checked) <- checkObject checks object
  return (higher, CosliceCat checked)
checkCategory checks (ProductCat cat cat') =
  checkCategoryPair checks ProductCat cat cat'
checkCategory checks (FunctorCat cat cat') =
  checkCategoryPair checks FunctorCat cat cat'
checkCategory _checks term@(AdjunctionCat _adj) =
  ExceptT.throwE $
    CheckUnimplemented (CategoryTerm term) "checking adjunction category"

checkCategoryPair ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  ( Category checkedCarrier ->
    Category checkedCarrier ->
    Category checkedCarrier
  ) ->
  Category uncheckedCarrier ->
  Category uncheckedCarrier ->
  CheckResultT
    m
    (HigherCategory checkedCarrier, Category checkedCarrier)
    uncheckedCarrier
checkCategoryPair checks combine cat cat' = do
  (higher, checked) <- checkCategory checks cat
  (higher', checked') <- checkCategory checks cat'
  catMatch <- equiv higher higher'
  unless catMatch $ ExceptT.throwE $ HigherCategoryMismatch cat cat'
  return (higher, combine checked checked')

instance (Monad m, Eq carrier) => Normalize m (Category carrier) where
  normalize = return

instance (Monad m, Eq carrier) => Equiv m (Category carrier) where
  equiv = normalizedEq

checkShape ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Shape uncheckedCarrier ->
  CheckResultT m (Shape checkedCarrier) uncheckedCarrier
checkShape _checks EmptyIndex = pure EmptyIndex
checkShape _checks SingletonIndex = pure SingletonIndex
checkShape _checks DiscretePair = pure DiscretePair
checkShape _checks ParallelPair = pure ParallelPair
checkShape _checks Span = pure Span
checkShape _checks Cospan = pure Cospan

instance (Monad m, Eq carrier) => Normalize m (Shape carrier) where
  normalize = return

instance (Monad m, Eq carrier) => Equiv m (Shape carrier) where
  equiv = normalizedEq

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
checkObject _checks term@(FMapObject _functor _object) =
  ExceptT.throwE $ CheckUnimplemented (ObjectTerm term) "checking functorApply"
checkObject _checks term@(Vertex _shape _index) =
  ExceptT.throwE $ CheckUnimplemented (ObjectTerm term) "checking Vertex"
checkObject _checks term@(ProductObject _shape _objects) =
  ExceptT.throwE $ CheckUnimplemented (ObjectTerm term) "checking ProductObject"
checkObject _checks term@(FunctorObject _functor) =
  ExceptT.throwE $ CheckUnimplemented (ObjectTerm term) "checking FunctorObject"

instance (Monad m, Eq carrier) => Normalize m (Object carrier) where
  normalize = return

instance (Monad m, Eq carrier) => Equiv m (Object carrier) where
  equiv = normalizedEq

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
  objMatch <- equiv fDom gsCod
  unless objMatch $
    ExceptT.throwE $ IllegalMorphismComposition f (ComposedMorphism g gs)
  return
    ( MinimalMetalogic,
      HigherOrderRefinedADTCat MinimalMetalogic,
      gsDom,
      fCod,
      ComposedMorphism f' [gs']
    )
checkMorphism _checks term@(FMapMorphism _functor _morphism) =
  ExceptT.throwE $
    CheckUnimplemented (MorphismTerm term) "checking FMapMorphism"
checkMorphism _checks term@(HigherMorphism _morphism) =
  ExceptT.throwE $
    CheckUnimplemented (MorphismTerm term) "checking HigherMorphism"
checkMorphism _checks term@(Edge _shape _index) =
  ExceptT.throwE $
    CheckUnimplemented (MorphismTerm term) "checking Edge"
checkMorphism _checks term@(ProductMorphism _shape _morphisms) =
  ExceptT.throwE $
    CheckUnimplemented (MorphismTerm term) "checking ProductMorphism"
checkMorphism _checks term@(AdjunctionUnit _adjunction _object) =
  ExceptT.throwE $
    CheckUnimplemented (MorphismTerm term) "checking AdjunctionUnit"
checkMorphism _checks term@(AdjunctionCounit _adjunction _object) =
  ExceptT.throwE $
    CheckUnimplemented (MorphismTerm term) "checking AdjunctionCounit"
checkMorphism _checks term@(ProjectMorphism _shape _object _carrier) =
  ExceptT.throwE $
    CheckUnimplemented (MorphismTerm term) "checking ProjectMorphism"

instance (Monad m, Eq carrier) => Normalize m (Morphism carrier) where
  normalize = return

instance (Monad m, Eq carrier) => Equiv m (Morphism carrier) where
  equiv = normalizedEq

checkDiagram ::
  ( Monad m,
    MinimalInstanceAlgebra uncheckedCarrier,
    MinimalInstanceAlgebra checkedCarrier
  ) =>
  AbstractChecks m uncheckedCarrier checkedCarrier ->
  Diagram uncheckedCarrier ->
  CheckResultT
    m
    ( HigherCategory checkedCarrier,
      Category checkedCarrier,
      Category checkedCarrier,
      Diagram checkedCarrier
    )
    uncheckedCarrier
checkDiagram _checks term =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm $ FunctorDiagram term) "checking diagram"

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
checkFunctor checks (DiagonalFunctor shape cat) = do
  shape' <- checkShape checks shape
  (higher, cat') <- checkCategory checks cat
  return (higher, IndexCat higher shape', cat', DiagonalFunctor shape' cat')
checkFunctor checks (ComposedFunctor f []) =
  checkFunctor checks f
checkFunctor checks (ComposedFunctor f (g : gs)) = do
  (higher, fDom, fCod, f') <-
    checkFunctor checks f
  (higher', gsDom, gsCod, gs') <-
    checkFunctor checks (ComposedFunctor g gs)
  catMatch <- equiv higher higher'
  unless catMatch $
    ExceptT.throwE $ IllegalFunctorComposition f (ComposedFunctor g gs)
  objMatch <- equiv fDom gsCod
  unless objMatch $
    ExceptT.throwE $ IllegalFunctorComposition f (ComposedFunctor g gs)
  return (higher, gsDom, fCod, ComposedFunctor f' [gs'])
checkFunctor _checks term@(ConstFunctor _obj) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking ConstFunctor"
checkFunctor _checks term@(LimitFunctor _shape _cat) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking LimitFunctor"
checkFunctor _checks term@(ColimitFunctor _shape _cat) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking ColimitFunctor"
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
checkFunctor checks (FunctorDiagram diagram) = do
  (higher, checked, checked', diagram') <- checkDiagram checks diagram
  return (higher, checked, checked', FunctorDiagram diagram')
checkFunctor _checks term@(BaseChangeFunctor _x _y) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking BaseChangeFunctor"
checkFunctor _checks term@(DependentProductFunctor _x _y) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking DependentProductFunctor"
checkFunctor _checks term@(DependentSumFunctor _x _y) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking DependentSumFunctor"
checkFunctor _checks term@(CobaseChangeFunctor _x _y) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking CobaseChangeFunctor"
checkFunctor _checks term@(CodependentProductFunctor _x _y) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking CodependentProductFunctor"
checkFunctor _checks term@(CodependentSumFunctor _x _y) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking CodependentSumFunctor"
checkFunctor _checks term@(FunctorCatObject _object) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking FunctorCatObject"
checkFunctor _checks term@(ProjectFunctor _shape _object _category) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking ProjectFunctor"
checkFunctor _checks term@(LeftAdjoint _adjunction) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking LeftAdjoint"
checkFunctor _checks term@(RightAdjoint _adjunction) =
  ExceptT.throwE $
    CheckUnimplemented (FunctorTerm term) "checking RightAdjoint"

instance (Monad m, Eq carrier) => Normalize m (Functor' carrier) where
  normalize = return

instance (Monad m, Eq carrier) => Equiv m (Functor' carrier) where
  equiv = normalizedEq

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
checkAdjunction checks (IdentityAdjunction cat) = do
  (higher, checked) <- checkCategory checks cat
  return
    ( higher,
      checked,
      checked,
      IdentityFunctor checked,
      IdentityFunctor checked,
      IdentityAdjunction checked
    )
checkAdjunction checks (ComposedAdjunction adj []) =
  checkAdjunction checks adj
checkAdjunction checks (ComposedAdjunction adj (adj' : adjs)) = do
  (higher, dom, cod, left, right, checked) <-
    checkAdjunction checks adj
  (higher', dom', cod', left', right', checked') <-
    checkAdjunction checks (ComposedAdjunction adj' adjs)
  catMatch <- equiv higher higher'
  unless catMatch $
    ExceptT.throwE $
      IllegalAdjunctionComposition adj (ComposedAdjunction adj' adjs)
  objMatch <- equiv dom cod'
  unless objMatch $
    ExceptT.throwE $
      IllegalAdjunctionComposition adj (ComposedAdjunction adj' adjs)
  return
    ( higher,
      dom',
      cod,
      ComposedFunctor left [left'],
      ComposedFunctor right' [right],
      ComposedAdjunction checked [checked']
    )
checkAdjunction _checks term@(LimitAdjunction _shape _cat) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "LimitAdjunction"
checkAdjunction _checks term@(ColimitAdjunction _shape _cat) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "ColimitAdjunction"
checkAdjunction _checks term@(FreeForgetfulAlgebra _obj) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "FreeForgetfulAlgebra"
checkAdjunction _checks term@(ForgetfulCofreeAlgebra _obj) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "ForgetfulCofreeAlgebra"
checkAdjunction _checks term@(ProductHomAdjunction _obj) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "ProductHomAdjunction"
checkAdjunction _checks term@(DependentSum _obj) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "DependentSum"
checkAdjunction _checks term@(DependentProduct _obj) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "DependentProduct"
checkAdjunction _checks term@(CodependentSum _obj) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "CodependentSum"
checkAdjunction _checks term@(CodependentProduct _obj) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "CodependentProduct"
checkAdjunction _checks term@(InitialAdjunction _category) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "InitialAdjunction"
checkAdjunction _checks term@(TerminalAdjunction _category) =
  ExceptT.throwE $
    CheckUnimplemented (AdjunctionTerm term) "TerminalAdjunction"

instance (Monad m, Eq carrier) => Normalize m (Adjunction carrier) where
  normalize = return

instance (Monad m, Eq carrier) => Equiv m (Adjunction carrier) where
  equiv = normalizedEq

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

instance (Monad m, Eq carrier) => Normalize m (HigherCategory carrier) where
  normalize = return

instance (Monad m, Eq carrier) => Equiv m (HigherCategory carrier) where
  equiv = normalizedEq

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
