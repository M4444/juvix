-- | This file contains the functions and aux functions to typecheck terms.
-- @typeTerm@ and @typeElim@ are called by functions for typechecking
-- datatype and function declarations.
module Juvix.Core.IR.CheckTerm
  ( module Juvix.Core.IR.CheckTerm,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty ((<|))
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Typechecker.Env as Env
import qualified Juvix.Core.IR.Typechecker.Error as Error
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library hiding (Datatype)
import qualified Juvix.Library.Usage as Usage
import Debug.Pretty.Simple (pTraceShowM, pTraceShow)
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Core.Base.Types.Globals as Globals 
data Leftovers a = Leftovers
  { loValue :: a,
    loLocals :: Env.UContext,
    loPatVars :: Env.PatUsages
  }
  deriving (Eq, Show, Generic)

type ShowExt ext primTy primVal =
  (Core.TermAll Show ext primTy primVal, Core.ElimAll Show ext primTy primVal)

leftoversOk :: Leftovers a -> Bool
leftoversOk (Leftovers {loLocals, loPatVars}) =
  all leftoverOk loLocals && all leftoverOk loPatVars

leftoverOk :: Usage.T -> Bool
leftoverOk ρ = ρ == Usage.SAny || ρ == mempty

starAnyAnn :: Usage.T -> Typed.AnnotationT IR.T primTy primVal
starAnyAnn σ = Typed.Annotation σ $ IR.VStar Core.UAny

-- | Checks a 'Term against an annotation and returns a decorated term if
-- successful.
typeTerm ::
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show ext,
    ShowExt ext primTy primVal,
    Env.CanTC' ext primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  Param.Parameterisation primTy primVal ->
  Core.Term ext primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  Eval.LookupFun IR.T primTy primVal ->
  m (Typed.Term primTy primVal)
typeTerm param t ann lookupGlobal = loValue <$> typeTermWith param IntMap.empty [] t ann lookupGlobal

typeTermWith ::
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show ext,
    ShowExt ext primTy primVal,
    Env.CanTC' ext primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  Param.Parameterisation primTy primVal ->
  Env.PatBinds primTy primVal ->
  Env.Context primTy primVal ->
  Core.Term ext primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  Eval.LookupFun IR.T primTy primVal ->
  m (Leftovers (Typed.Term primTy primVal))
typeTermWith param pats ctx t ann lookupGlobal =
  Env.execInner (withLeftovers $ typeTerm' t ann lookupGlobal) (Env.InnerState param pats ctx)

-- | Infers the type and usage for an 'Elim and returns it decorated with this
-- information.
typeElim ::
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show ext,
    ShowExt ext primTy primVal,
    Env.CanTC' ext primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  Param.Parameterisation primTy primVal ->
  Core.Elim ext primTy primVal ->
  Usage.T ->
  Eval.LookupFun IR.T primTy primVal ->
  m (Typed.Elim primTy primVal)
typeElim param e σ lookupGlobal =
  loValue <$> typeElimWith param IntMap.empty [] e σ lookupGlobal

typeElimWith ::
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show ext,
    ShowExt ext primTy primVal,
    Env.CanTC' ext primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  Param.Parameterisation primTy primVal ->
  Env.PatBinds primTy primVal ->
  Env.Context primTy primVal ->
  Core.Elim ext primTy primVal ->
  Usage.T ->
  Eval.LookupFun IR.T primTy primVal ->
  m (Leftovers (Typed.Elim primTy primVal))
typeElimWith param pats ctx e σ lookupGlobal =
  Env.execInner (withLeftovers $ typeElim' e σ lookupGlobal) (Env.InnerState param pats ctx)

withLeftovers ::
  (Env.HasBound primTy primVal m, Env.HasPatBinds primTy primVal m) =>
  m a ->
  m (Leftovers a)
withLeftovers m =
  Leftovers <$> m
    <*> fmap (fmap Typed.annUsage) (get @"bound")
    <*> fmap (fmap Typed.annUsage) (get @"patBinds")

typeTerm' ::
  ( Eq primTy,
    Eq primVal,
    Show primVal,
    Show ext,
    ShowExt ext primTy primVal,
    (Show (Core.XAnn ext primTy primVal)),
    Show primTy,
    (Show (Core.ElimX ext primTy primVal)),
    Env.CanInnerTC' ext primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  Core.Term ext primTy primVal ->
  Typed.AnnotationT IR.T primTy primVal ->
  Eval.LookupFun IR.T primTy primVal ->
  m (Typed.Term primTy primVal)
typeTerm' term ann@(Typed.Annotation σ ty) lookupGlobal =
  case term of
    Core.Star i _ -> do
      requireZero σ
      j <- requireStar ty
      requireUniverseLT i j
      pure $ Typed.Star i ann
    Core.PrimTy t _ -> do
      requireZero σ
      void $ requirePrimStars (Param.primArity t) ty
      let t' =
            App.Return
              { retTerm = t,
                retType = Param.getPrimTypeKind t
              }
      pure $ Typed.PrimTy t' ann
    Core.Prim p _ -> do
      p' <- typePrim p ty
      pure $ Typed.Prim p' $ Typed.Annotation σ ty
    Core.Pi π a b _ -> do
      requireZero σ
      void $ requireStar ty
      a' <- typeTerm' a ann lookupGlobal
      av <- evalTC a'
      b' <- withLocal (Typed.Annotation mempty av) $ typeTerm' b ann lookupGlobal
      pure $ Typed.Pi π a' b' ann
    Core.Lam t _ -> do
      (π, a, b) <- requirePi ty
      let varAnn = Typed.Annotation (σ <.> π) a
          tAnn = Typed.Annotation σ b
      t' <- withLocal varAnn $ typeTerm' t tAnn lookupGlobal
      let anns = Typed.BindAnnotation {baBindAnn = varAnn, baResAnn = ann}
      pure $ Typed.Lam t' anns
    Core.Sig π a b _ -> do
      requireZero σ
      void $ requireStar ty
      a' <- typeTerm' a ann lookupGlobal
      av <- evalTC a'
      b' <- withLocal (Typed.Annotation mempty av) $ typeTerm' b ann lookupGlobal
      pure $ Typed.Sig π a' b' ann
    Core.Pair s t _ -> do
      (π, a, b) <- requireSig ty
      let sAnn = Typed.Annotation (σ <.> π) a
      s' <- typeTerm' s sAnn lookupGlobal
      tAnn <- Typed.Annotation σ <$> substApp b s'
      t' <- typeTerm' t tAnn lookupGlobal
      pure $ Typed.Pair s' t' ann
    Core.CatProduct a b _ -> do
      requireZero σ
      void $ requireStar ty
      a' <- typeTerm' a ann lookupGlobal
      b' <- typeTerm' b ann lookupGlobal
      pure $ Typed.CatProduct a' b' ann
    Core.CatCoproduct a b _ -> do
      requireZero σ
      void $ requireStar ty
      a' <- typeTerm' a ann lookupGlobal
      b' <- typeTerm' b ann lookupGlobal
      pure $ Typed.CatCoproduct a' b' ann
    Core.CatProductIntro s t _ -> do
      (π, a, b) <- requireCatProduct ty
      let sAnn = Typed.Annotation (σ <.> π) a
      let tAnn = Typed.Annotation (σ <.> π) b
      s' <- typeTerm' s sAnn lookupGlobal
      t' <- typeTerm' t tAnn lookupGlobal
      pure $ Typed.CatProductIntro s' t' ann
    Core.CatProductElimLeft a s _ -> do
      a' <- typeTerm' a (starAnyAnn σ) lookupGlobal
      av <- evalTC a'
      let sAnn = Typed.Annotation σ (IR.VCatProduct ty av)
      s' <- typeTerm' s sAnn lookupGlobal
      pure $ Typed.CatProductElimLeft a' s' ann
    Core.CatProductElimRight a s _ -> do
      a' <- typeTerm' a (starAnyAnn σ) lookupGlobal
      av <- evalTC a'
      let sAnn = Typed.Annotation σ (IR.VCatProduct av ty)
      s' <- typeTerm' s sAnn lookupGlobal
      pure $ Typed.CatProductElimRight a' s' ann
    Core.CatCoproductIntroLeft s _ -> do
      (π, a, _b) <- requireCatCoproduct ty
      let sAnn = Typed.Annotation (σ <.> π) a
      s' <- typeTerm' s sAnn lookupGlobal
      pure $ Typed.CatCoproductIntroLeft s' ann
    Core.CatCoproductIntroRight s _ -> do
      (π, _a, b) <- requireCatCoproduct ty
      let sAnn = Typed.Annotation (σ <.> π) b
      s' <- typeTerm' s sAnn lookupGlobal
      pure $ Typed.CatCoproductIntroRight s' ann
    Core.CatCoproductElim a b cp s t _ -> do
      a' <- typeTerm' a (starAnyAnn σ) lookupGlobal
      av <- evalTC a'
      b' <- typeTerm' b (starAnyAnn σ) lookupGlobal
      bv <- evalTC b'
      cp' <- typeTerm' cp (Typed.Annotation σ (IR.VCatCoproduct av bv)) lookupGlobal
      s' <- typeTerm' s (Typed.Annotation σ (IR.VPi σ av ty)) lookupGlobal
      t' <- typeTerm' t (Typed.Annotation σ (IR.VPi σ bv ty)) lookupGlobal
      pure $ Typed.CatCoproductElim a' b' cp' s' t' ann
    Core.UnitTy _ -> do
      requireZero σ
      void $ requireStar ty
      pure $ Typed.UnitTy ann
    Core.Unit _ -> do
      requireUnitTy ty
      pure $ Typed.Unit ann
    Core.Let σb b t _ -> do
      b' <- typeElim' b σb lookupGlobal
      let bAnn = Typed.getElimAnn b'
          tAnn = Typed.Annotation σ (Eval.weak ty)
      t' <- withLocal bAnn $ typeTerm' t tAnn lookupGlobal
      let anns = Typed.BindAnnotation {baBindAnn = bAnn, baResAnn = ann}
      pure $ Typed.Let σb b' t' anns
    Core.Elim e _ -> do
      e' <- typeElim' e σ lookupGlobal
      let ty' = Typed.annType $ Typed.getElimAnn e'
      requireSubtype e ty ty'
      pure $ Typed.Elim e' ann
    Core.TermX x ->
      Error.throwTC $ Error.UnsupportedTermExt x

typeElim' ::
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show ext,
    ShowExt ext primTy primVal,
    Env.CanInnerTC' ext primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  Core.Elim ext primTy primVal ->
  Usage.T ->
  Eval.LookupFun IR.T primTy primVal ->
  m (Typed.Elim primTy primVal)
typeElim' elim σ lookupGlobal =
  case elim of
    Core.Bound i _ -> do
      ty <- useLocal σ i
      pure $ Typed.Bound i $ Typed.Annotation σ ty
    Core.Free px@(Core.Pattern x) _ -> do
      ty <- usePatVar σ x
      pure $ Typed.Free px $ Typed.Annotation σ ty
    Core.Free gx@(Core.Global x) _ -> do
      (ty, π') <- Env.lookupGlobal x
      when (π' == Core.GZero) $ requireZero σ
      pure $ Typed.Free gx $ Typed.Annotation σ ty
    Core.CaseTree caseTree _ -> do
      Typed.CaseTree <$> typeCaseTree' caseTree σ lookupGlobal <*> pure (Typed.Annotation mempty (IR.VStar Core.UAny))
    Core.App s t _ -> do
      s' <- typeElim' s σ lookupGlobal
      (π, a, b) <- requirePi $ Typed.annType $ Typed.getElimAnn s'
      let tAnn = Typed.Annotation (σ <.> π) a
      t' <- typeTerm' t tAnn lookupGlobal
      ty <- substApp b t'
      pure $ Typed.App s' t' $ Typed.Annotation σ ty
    Core.Ann π s a _ -> do
      a' <- typeTerm' a (Typed.Annotation mempty (IR.VStar Core.UAny)) lookupGlobal
      ty <- evalTC a'
      let ann = Typed.Annotation σ ty
      s' <- typeTerm' s ann lookupGlobal
      pure $ Typed.Ann π s' a' ann
    Core.ElimX x ->
      Error.throwTC $ Error.UnsupportedElimExt x

typePattern' ::
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show ext,
    ShowExt ext primTy primVal,
    Env.CanInnerTC' ext primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  Core.Pattern ext primTy primVal ->
  Usage.T ->
  Eval.LookupFun IR.T primTy primVal ->
  m (Typed.Pattern primTy primVal)
typePattern' patttern σ lookupGlobal =
  let anyTy = IR.VStar Core.UAny
      anyAnn = Typed.Annotation mempty anyTy
  in case patttern of
    Core.PCon name patterns _ -> do
      patterns' <- traverse (\pat -> typePattern' pat σ lookupGlobal) patterns
      -- TODO: Lookup name
      -- TODO: Append signature to "patBinds"
      updatePatBinds name 
      pure $ Typed.PCon name patterns' anyAnn
    Core.PPair pat1 pat2 _ -> do
      tPat1 <- typePattern' pat1 σ lookupGlobal
      tPat2 <- typePattern' pat2 σ lookupGlobal
      pure $ Typed.PPair tPat1 tPat2 anyAnn
    Core.PUnit _ -> pure $ Typed.PUnit anyAnn
    Core.PVar pvar _ -> pure $ Typed.PVar pvar anyAnn
    Core.PDot term _ -> Typed.PDot <$> typeTerm' term anyAnn lookupGlobal <*> pure anyAnn
    Core.PPrim p _ -> do
      p' <- typePrim p anyTy
      pure $ Typed.PPrim p' anyAnn
    where
      updatePatBinds name = do
        notImplemented 


typeBranch' ::
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show ext,
    ShowExt ext primTy primVal,
    Env.CanInnerTC' ext primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  Core.Branch ext primTy primVal ->
    -- TODO: Pass the type of the pattern
  Usage.T ->
  Eval.LookupFun IR.T primTy primVal ->
  m (Typed.Branch primTy primVal)
typeBranch' (Core.Branch pat caseTree _) σ lookupGlobal =
  let anyAnn = Typed.Annotation mempty (IR.VStar Core.UAny)
  in Typed.Branch <$> typePattern' pat σ lookupGlobal <*> typeCaseTree' caseTree σ lookupGlobal <*> pure anyAnn <*> pure anyAnn

typeCaseTree' ::
  ( Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show ext,
    ShowExt ext primTy primVal,
    Env.CanInnerTC' ext primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy
  ) =>
  Core.CaseTree ext primTy primVal ->
  Usage.T ->
  Eval.LookupFun IR.T primTy primVal ->
  m (Typed.CaseTree primTy primVal)
typeCaseTree' caseTree σ lookupGlobal =
  let anyAnn = Typed.Annotation mempty (IR.VStar Core.UAny)
  in case caseTree of
    Core.Case arg branches _ -> do
      branches' <- traverse (\branch -> typeBranch' branch σ lookupGlobal) branches
      pure $ Typed.Case arg branches' anyAnn
    Core.Done args t _ -> do
      t' <- typeTerm' t anyAnn lookupGlobal
      pure $ Typed.Done args t' anyAnn
    Core.Fail args _ -> do
      pure $ Typed.Fail args anyAnn


pushLocal ::
  Env.HasBound primTy primVal m =>
  Typed.AnnotationT IR.T primTy primVal ->
  m ()
pushLocal ann = modify @"bound" (ann :)

popLocal ::
  ( Env.HasBound primTy primVal m,
    Error.HasThrowTC' IR.T ext primTy primVal m
  ) =>
  m ()
popLocal = do
  ctx <- get @"bound"
  case ctx of
    Typed.Annotation ρ _ : ctx -> do
      unless (leftoverOk ρ) $ Error.throwTC (Error.LeftoverUsage ρ)
      put @"bound" ctx
    [] -> do
      Error.throwTC (Error.UnboundLocal 0)

withLocal ::
  ( Env.HasBound primTy primVal m,
    Error.HasThrowTC' IR.T ext primTy primVal m
  ) =>
  Typed.AnnotationT IR.T primTy primVal ->
  m a ->
  m a
withLocal ann m = pushLocal ann *> m <* popLocal

requireZero ::
  Error.HasThrowTC' IR.T ext primTy primVal m =>
  Usage.T ->
  m ()
requireZero π =
  unless (π == mempty) $
    Error.throwTC (Error.InsufficientUsage π (Usage.SNat 0))

requireStar ::
  Error.HasThrowTC' IR.T ext primTy primVal m =>
  Typed.ValueT IR.T primTy primVal ->
  m Core.Universe
requireStar (IR.VStar j) = pure j
requireStar ty = Error.throwTC (Error.ShouldBeStar ty)

-- | Given the arity of a type, require all of them to be Star. We expect the
-- arguments to be written in a right-associative way, e.g: @* -> (* -> *)@.
requirePrimStars ::
  Error.HasThrowTC' IR.T ext primTy primVal m =>
  Natural ->
  Typed.ValueT IR.T primTy primVal ->
  m Core.Universe
requirePrimStars 0 ty = requireStar ty
requirePrimStars n ty = do
  (_, l, r) <- requirePi ty
  void $ requireStar l
  requirePrimStars (n - 1) r

requireUniverseLT ::
  Error.HasThrowTC' IR.T ext primTy primVal m =>
  Core.Universe ->
  Core.Universe ->
  m ()
requireUniverseLT i j = unless (i < j) $ Error.throwTC (Error.UniverseMismatch i j)

typePrim ::
  Env.CanInnerTC' ext primTy primVal m =>
  primVal ->
  Typed.ValueT IR.T primTy primVal ->
  m (Param.TypedPrim primTy primVal)
typePrim p ty = do
  param <- ask @"param"
  ty' <- toPrimTy ty
  if (Param.hasType param p ty')
    then pure $ App.Return {retType = ty', retTerm = p}
    else Error.throwTC $ Error.WrongPrimTy p ty'

toPrimTy ::
  Env.CanInnerTC' ext primTy primVal m =>
  Typed.ValueT IR.T primTy primVal ->
  m (Param.PrimType primTy)
toPrimTy ty =
  maybe
    (Error.throwTC $ Error.NotPrimTy ty)
    (pure . Param.PrimType)
    $ go ty
  where
    go (IR.VPrimTy t) = (:| []) <$> groundPrimTy t
    go (IR.VPi _ (IR.VPrimTy s) t) = (<|) <$> groundPrimTy s <*> go t
    go _ = empty

-- FIXME support variables too
groundPrimTy :: Alternative f => Typed.PrimTy primTy -> f primTy
groundPrimTy (App.Cont {}) = empty
groundPrimTy (App.Return {retTerm}) = pure retTerm

type TyParts primTy primVal =
  (Usage.T, Typed.ValueT IR.T primTy primVal, Typed.ValueT IR.T primTy primVal)

requirePi ::
  Error.HasThrowTC' IR.T ext primTy primVal m =>
  Typed.ValueT IR.T primTy primVal ->
  m (TyParts primTy primVal)
requirePi (IR.VPi π a b) = pure (π, a, b)
requirePi ty = Error.throwTC (Error.ShouldBeFunctionType ty)

requireSig ::
  Error.HasThrowTC' IR.T ext primTy primVal m =>
  Typed.ValueT IR.T primTy primVal ->
  m (TyParts primTy primVal)
requireSig (IR.VSig π a b) = pure (π, a, b)
requireSig ty = Error.throwTC (Error.ShouldBePairType ty)

requireCatProduct ::
  Error.HasThrowTC' IR.T ext primTy primVal m =>
  Typed.ValueT IR.T primTy primVal ->
  m (TyParts primTy primVal)
requireCatProduct (IR.VCatProduct a b) = pure (Usage.SAny, a, b)
requireCatProduct ty = Error.throwTC (Error.ShouldBeCatProductType ty)

requireCatCoproduct ::
  Error.HasThrowTC' IR.T ext primTy primVal m =>
  Typed.ValueT IR.T primTy primVal ->
  m (TyParts primTy primVal)
requireCatCoproduct (IR.VCatCoproduct a b) = pure (Usage.SAny, a, b)
requireCatCoproduct ty = Error.throwTC (Error.ShouldBeCatCoproductType ty)

requireUnitTy ::
  Error.HasThrowTC' IR.T ext primTy primVal m =>
  Typed.ValueT IR.T primTy primVal ->
  m ()
requireUnitTy IR.VUnitTy = pure ()
requireUnitTy ty = Error.throwTC (Error.ShouldBeUnitType ty)

requireSubtype ::
  (Eq primTy, Eq primVal, Error.HasThrowTC' IR.T ext primTy primVal m) =>
  Core.Elim ext primTy primVal ->
  Typed.ValueT IR.T primTy primVal ->
  Typed.ValueT IR.T primTy primVal ->
  m ()
requireSubtype subj exp got =
  unless (got <: exp) $ Error.throwTC (Error.TypeMismatch subj exp got)

useLocal ::
  ( Env.HasBound primTy primVal m,
    Error.HasThrowTC' IR.T ext primTy primVal m,
    Eval.HasWeak primTy,
    Eval.HasWeak primVal
  ) =>
  Usage.T ->
  Core.BoundVar ->
  m (Typed.ValueT IR.T primTy primVal)
useLocal π var = do
  ctx <- get @"bound"
  (ty, ctx) <- go 1 var ctx
  put @"bound" ctx
  pure ty
  where
    go _ _ [] = Error.throwTC (Error.UnboundLocal var)
    go w 0 (Typed.Annotation ρ ty : ctx) = do
      case ρ `Usage.minus` π of
        Just ρ' -> pure (Eval.weakBy w ty, Typed.Annotation ρ' ty : ctx)
        Nothing -> Error.throwTC (Error.InsufficientUsage π ρ)
    go w i (b : ctx) = second (b :) <$> go (w + 1) (i - 1) ctx

usePatVar ::
  ( Env.HasPatBinds primTy primVal m,
    Error.HasThrowTC' IR.T ext primTy primVal m,
    Show primTy, Show primVal
  ) =>
  Usage.T ->
  Core.PatternVar ->
  m (Typed.ValueT IR.T primTy primVal)
usePatVar π var = do
  -- TODO a single traversal with alterF or something
  mAnn <- gets @"patBinds" $ IntMap.lookup var
  patBinds <- get @"patBinds"
  case pTraceShow ("patBinds", patBinds) mAnn of
    Just (Typed.Annotation ρ ty)
      | Just ρ' <- ρ `Usage.minus` π -> do
        modify @"patBinds" $ IntMap.insert var $ Typed.Annotation ρ' ty
        pure ty
      | otherwise -> do
        Error.throwTC (Error.InsufficientUsage π ρ)
    Nothing -> do
      Error.throwTC (Error.UnboundPatVar var)

type TCEvalError primTy primVal =
  Eval.Error
    IR.T
    Typed.T
    (Param.KindedType primTy)
    (Param.TypedPrim primTy primVal)

liftEval ::
  Error.HasThrowTC' extV extT primTy primVal m =>
  Either (TCEvalError primTy primVal) a ->
  m a
liftEval = either (Error.throwTC . Error.EvalError) pure

substApp ::
  ( Env.HasParam primTy primVal m,
    Error.HasThrowTC' IR.T extT primTy primVal m,
    Env.HasGlobals primTy primVal m,
    Eval.HasWeak primVal,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Env.PrimSubstValue primTy primVal,
    Env.PrimPatSubstTerm primTy primVal,
    Eval.HasPatSubstType
      (OnlyExts.T Typed.T)
      primTy
      (Param.TypedPrim primTy primVal)
      primTy,
    Show primVal,
    Show primTy
  ) =>
  Typed.ValueT IR.T primTy primVal ->
  Typed.Term primTy primVal ->
  m (Typed.ValueT IR.T primTy primVal)
substApp ty arg = do
  arg' <- evalTC arg
  liftEval $ first Eval.ErrorValue (Eval.substV arg' ty)

evalTC ::
  ( Error.HasThrowTC' IR.T ext primTy primVal m,
    Env.HasGlobals primTy primVal m,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Eval.HasWeak primTy,
    Eval.HasWeak primVal,
    Env.PrimSubstValue primTy primVal,
    Env.PrimPatSubstTerm primTy primVal,
    Show primVal,
    Show primTy
  ) =>
  Typed.Term primTy primVal ->
  m (Typed.ValueT IR.T primTy primVal)
evalTC t = do
  g <- ask @"globals"
  liftEval $ Eval.evalTerm (Eval.lookupFun @Typed.T g) t

-- | Subtyping. If @s <: t@ then @s@ is a subtype of @t@, i.e. everything of
-- type @s@ can also be checked against type @t@.
--
-- Currently subtyping consists of the following:
--
-- * Consistency of universe levels (@*ᵢ <: *ⱼ@ if @i ≤ j@)
-- * Usage compatibility (@(π x: A) → B <: (ω x: A) → B@ for finite @π@)
-- * Contravariant domain & covariant codomain
--   (@(π x: A₁) → B₁ <: (π x: A₂) → B₂@ if
--    @A₂ <: A₁@ and @B₁ <: B₂@)
-- * Covariance in both parts of Σ
-- * It doesn't descend into any other structures
--   (TODO: which ones are safe to do so?)
--
-- NB. Levels are currently not checked!
(<:) ::
  ( Eq primTy,
    Eq primVal,
    Core.ValueAll Eq ext primTy primVal,
    Core.NeutralAll Eq ext primTy primVal
  ) =>
  Core.Value ext primTy primVal ->
  Core.Value ext primTy primVal ->
  Bool
Core.VStar i _ <: Core.VStar j _ = i <= j
Core.VPi π1 s1 t1 _ <: Core.VPi π2 s2 t2 _ =
  π2 `Usage.allows` π1 && s2 <: s1 && t1 <: t2
Core.VSig π1 s1 t1 _ <: Core.VSig π2 s2 t2 _ =
  -- TODO is this right???
  π1 `Usage.allows` π2
    && s1 <: s2
    && t1 <: t2
s1 <: s2 = s1 == s2

infix 4 <: -- same as (<), etc
