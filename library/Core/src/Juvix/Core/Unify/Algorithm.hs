{-# LANGUAGE ViewPatterns #-}

module Juvix.Core.Unify.Algorithm
  ( module Juvix.Core.Unify.Types,
    UnifyResult,
    Success (..),
    unifyV,
    unifyN,
    outsideVars,
    outsideVarsN,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Juvix.Core.Base.Types.Globals as Core
import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Unify.MetaVar as Meta
import Juvix.Core.Unify.Types
import Juvix.Library
import qualified Juvix.Library.Usage as Usage

type UnifyResult primTy primVal =
  Either (Error primTy primVal) (Success primTy primVal)

data Success primTy primVal = Success
  { subst :: Subst primTy primVal,
    unsolved :: MetaSet
  }

unifyV ::
  PrimUnify primTy primVal =>
  Core.Globals IR.T IR.T primTy primVal ->
  Value primTy primVal ->
  Value primTy primVal ->
  UnifyResult primTy primVal
unifyV globals s t = runUnify globals do
  checkOutsideVars s t
  addProblem s t
  doUnify
  Success <$> get @"subst" <*> getUnsolved s t

unifyN ::
  PrimUnify primTy primVal =>
  Core.Globals IR.T IR.T primTy primVal ->
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  UnifyResult primTy primVal
unifyN globals = unifyV globals `on` VNeutral

doUnify :: CanUnify primTy primVal m => m ()
doUnify =
  nextProblem >>= \case
    Just (s, t) -> unify1 s t >> doUnify
    Nothing -> pure ()

-- FIXME make adjustments when going under binders
unify1 ::
  CanUnify primTy primVal m =>
  Value primTy primVal ->
  Value primTy primVal ->
  m ()
unify1 (VMeta α) (VMeta β) = unless (α == β) $ bind α (VMeta β)
unify1 (VMeta α) t = tryBind α t
unify1 s (VMeta α) = tryBind α s
unify1 s@(VStar 𝓀) t@(VStar ℓ) = eqAtom (𝓀, s) (ℓ, t)
unify1 s@(VPrimTy a) t@(VPrimTy b) = eqAtom (a, s) (b, t)
unify1 (VPi π a b) (VPi ρ c d) = unifyBinder (π, a, b) (ρ, c, d)
unify1 (VLam s) (VLam t) = addProblem s t
unify1 (VSig π a b) (VSig ρ c d) = unifyBinder (π, a, b) (ρ, c, d)
unify1 (VPair s t) (VPair u v) = addProblems [(s, t), (u, v)]
unify1 VUnitTy VUnitTy = pure ()
unify1 VUnit VUnit = pure ()
unify1 (VNeutral e) (VNeutral f) = unify1N e f
unify1 s@(VPrim p) t@(VPrim q) = eqAtom (p, s) (q, t)
unify1 s t = clash s t

unify1N ::
  CanUnify primTy primVal m =>
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  m ()
unify1N (NApps f ss) (NApps g ts) = unifyApp (f, ss) (g, ts)
unify1N e@(NBound i) f@(NBound j) = eqAtomN (i, e) (j, f)
unify1N e@(NFree x) f@(NFree y) = eqAtomN (x, e) (y, f)
unify1N e f = clashN e f

pattern NApps ::
  Neutral primTy primVal ->
  [Value primTy primVal] ->
  Neutral primTy primVal
pattern NApps f ss <-
  (getNApps -> (f, ss@(_ : _)))
  where
    NApps f ss = foldl NApp f ss

getNApps ::
  Neutral primTy primVal ->
  (Neutral primTy primVal, [Value primTy primVal])
getNApps n = go n []
  where
    go (NApp f s) acc = go f (s : acc)
    go f acc = (f, acc)

tryBind ::
  ( HasWeak primTy,
    HasWeak primVal,
    HasError primTy primVal m,
    HasProblems primTy primVal m,
    HasSubst primTy primVal m
  ) =>
  MetaVar ->
  Value primTy primVal ->
  m ()
tryBind α t = occursCheck α t >> bind α t

unifyApp ::
  ( Eq primTy,
    Eq primVal,
    HasError primTy primVal m,
    HasGlobals primTy primVal m,
    HasProblems primTy primVal m
  ) =>
  (Neutral primTy primVal, [Value primTy primVal]) ->
  (Neutral primTy primVal, [Value primTy primVal]) ->
  m ()
unifyApp (f, ss) (g, ts) = do
  eqNeut f g
  inj <- isInjective f
  let unif' = if inj then addProblem else eqVal
  sequence_ $ zipWith unif' ss ts

unifyBinder ::
  (HasError primTy primVal m, HasProblems primTy primVal m) =>
  (Usage.T, Value primTy primVal, Value primTy primVal) ->
  (Usage.T, Value primTy primVal, Value primTy primVal) ->
  m ()
unifyBinder (π, a, b) (ρ, c, d) = do
  eqUsage π ρ
  addProblems [(a, c), (b, d)]

isInjective :: HasGlobals primTy primVal m => Neutral primTy primVal -> m Bool
isInjective (NFree (Global x)) =
  asks @"globals" (HashMap.lookup x) >>| \case
    Just (Core.GDataCon _) -> True
    _ -> False
isInjective _ = pure False

getUnsolved ::
  (HasSubst primTy primVal m, HasProblems primTy primVal m) =>
  Value primTy primVal ->
  Value primTy primVal ->
  m MetaSet
getUnsolved s t = do
  sub <- gets @"subst" getSubst
  probs <- get @"problems"
  let go = Meta.filterS (not . (`Meta.memberM` sub)) . metasV
  let unsS = foldMap go sub
  let unsP = foldMap (\(p, q) -> go p <> go q) probs
  pure $ go s <> go t <> unsS <> unsP

eqVal ::
  (Eq primTy, Eq primVal, HasError primTy primVal m) =>
  Value primTy primVal ->
  Value primTy primVal ->
  m ()
eqVal s t = unless (s == t) $ clash s t

eqNeut ::
  (Eq primTy, Eq primVal, HasError primTy primVal m) =>
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  m ()
eqNeut = eqVal `on` VNeutral

eqUsage :: HasError primTy primVal m => Usage.T -> Usage.T -> m ()
eqUsage π ρ = unless (π == ρ) $ clashU π ρ

eqAtom ::
  (Eq a, HasError primTy primVal m) =>
  (a, Value primTy primVal) ->
  (a, Value primTy primVal) ->
  m ()
eqAtom (x, s) (y, t) = unless (x == y) $ clash s t

eqAtomN ::
  (Eq a, HasError primTy primVal m) =>
  (a, Neutral primTy primVal) ->
  (a, Neutral primTy primVal) ->
  m ()
eqAtomN = eqAtom `on` second VNeutral

clash ::
  HasError primTy primVal m =>
  Value primTy primVal ->
  Value primTy primVal ->
  m ()
clash s t = throwU $ Clash s t

clashN ::
  HasError primTy primVal m =>
  Neutral primTy primVal ->
  Neutral primTy primVal ->
  m ()
clashN = clash `on` VNeutral

clashU :: HasError primTy primVal m => Usage.T -> Usage.T -> m ()
clashU π ρ = throwU $ ClashU π ρ

checkOutsideVars ::
  HasError primTy primVal m =>
  Value primTy primVal ->
  Value primTy primVal ->
  m ()
checkOutsideVars s t =
  unless (Set.null outside) $ throwU $ OutsideVariables outside
  where
    outside = outsideVars s <> outsideVars t

outsideVars :: Value primTy primVal -> Set BoundVar
outsideVars = outsideVars' 0

outsideVars' :: BoundVar -> Value primTy primVal -> Set BoundVar
outsideVars' i = \case
  VStar _ -> mempty
  VPrimTy _ -> mempty
  VPi _ a b -> outsideVars' i a <> outsideVars' (i + 1) b
  VLam t -> outsideVars' (i + 1) t
  VSig _ a b -> outsideVars' i a <> outsideVars' (i + 1) b
  VPair s t -> outsideVars' i s <> outsideVars' i t
  VCatProduct a b -> outsideVars' i a <> outsideVars' i b
  VCatCoproduct a b -> outsideVars' i a <> outsideVars' i b
  VCatProductIntro s t -> outsideVars' i s <> outsideVars' i t
  VCatProductElimLeft b s -> outsideVars' i b <> outsideVars' i s
  VCatProductElimRight a s -> outsideVars' i a <> outsideVars' i s
  VCatCoproductIntroLeft s -> outsideVars' i s
  VCatCoproductIntroRight t -> outsideVars' i t
  VCatCoproductElim a b s t e -> foldMap (outsideVars' i) [a, b, s, t, e]
  VUnitTy -> mempty
  VUnit -> mempty
  VPrim _ -> mempty
  VNeutral n -> outsideVarsN' i n
  VMeta _ -> mempty

outsideVarsN :: Neutral primTy primVal -> Set BoundVar
outsideVarsN = outsideVarsN' 0

outsideVarsN' :: BoundVar -> Neutral primTy primVal -> Set BoundVar
outsideVarsN' i = \case
  NBound j
    | j >= i -> Set.singleton (j - i)
    | otherwise -> mempty
  NFree _ -> mempty
  NApp f s -> outsideVarsN' i f <> outsideVars' i s
