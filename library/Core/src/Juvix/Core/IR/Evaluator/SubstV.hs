{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module providing the `HasSubstV`-class, implementing substitution of
-- values.
module Juvix.Core.IR.Evaluator.SubstV
  ( HasSubstValue (..),
    HasSubstValueType (..),
    HasSubstV (..),
    substV,
    vapp,
    vrecelim,
  )
where

import qualified Juvix.Core.Application as App
import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.IR.Evaluator.Types
import Juvix.Core.IR.Evaluator.Weak
import qualified Juvix.Core.IR.Typechecker.Types as Typed
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import Control.Comonad
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

-- | Class of values that support substitution, allows failure using @Either@.
class HasWeak a => HasSubstV extV primTy primVal a where
  substVWith ::
    -- | How many bindings have been traversed so far.
    Natural ->
    -- | Variable to substitute.
    Core.BoundVar ->
    -- | Value to substitute with.
    Core.Value extV primTy primVal ->
    -- | Term to perform substitution on.
    a ->
    Either (ErrorValue extV primTy primVal) a
  default substVWith ::
    ( Generic a,
      GHasSubstV extV primTy primVal (Rep a)
    ) =>
    Natural ->
    Core.BoundVar ->
    Core.Value extV primTy primVal ->
    a ->
    Either (ErrorValue extV primTy primVal) a
  substVWith b i e = fmap to . gsubstVWith b i e . from

-- | Wrapper around `substWith` for toplevel terms without free variables.
substV' ::
  HasSubstV extV primTy primVal a =>
  Core.BoundVar ->
  Core.Value extV primTy primVal ->
  a ->
  Either (ErrorValue extV primTy primVal) a
substV' = substVWith 0

-- | Wrapper around `substV'` that starts at variable 0, the first bound
substV ::
  HasSubstV extV primTy primVal a =>
  Core.Value extV primTy primVal ->
  a ->
  Either (ErrorValue extV primTy primVal) a
substV = substV' 0

-- | Class of terms that support substitution, resulting in a `IR.Value`.
class HasWeak a => HasSubstValue extV primTy primVal a where
  substValueWith ::
    -- | How many bindings have been traversed so far.
    Natural ->
    -- | Variable to substitute.
    Core.BoundVar ->
    -- | Value to substitute with.
    Core.Value extV primTy primVal ->
    a ->
    Either (ErrorValue extV primTy primVal) (Core.Value extV primTy primVal)

-- | Class of terms that support substitution, resulting in a `IR.Value`.
class HasWeak a => HasSubstValueType extV primTy primVal a where
  substValueTypeWith ::
    -- | How many bindings have been traversed so far.
    Natural ->
    -- | Variable to substitute.
    Core.BoundVar ->
    -- | Value to substitute with.
    Core.Value extV primTy primVal ->
    a ->
    Either (ErrorValue extV primTy primVal) (Core.Value extV primTy primVal)

type ShowAllV extV primTy primVal =
  ( Core.ValueAll Show extV primTy primVal,
    Core.NeutralAll Show extV primTy primVal,
    Show primVal,
    Show primTy
  )

-- | Constraint alias for values and neutrals that support substitution.
type AllSubstV extV primTy primVal =
  ( Core.ValueAll (HasSubstV extV primTy primVal) extV primTy primVal,
    Core.NeutralAll (HasSubstV extV primTy primVal) extV primTy primVal,
    HasSubstValueType extV primTy primVal primTy,
    HasSubstValue extV primTy primVal primVal,
    Param.CanApply primTy,
    Param.CanApply primVal,
    ShowAllV extV primTy primVal
  )

instance
  ( AllSubstV extV primTy primVal,
    Monoid (Core.XVNeutral extV primTy primVal),
    Monoid (Core.XVLam extV primTy primVal),
    Monoid (Core.XVPrimTy extV primTy primVal),
    Monoid (Core.XVPrim extV primTy primVal)
  ) =>
  HasSubstV extV primTy primVal (Core.Value extV primTy primVal)
  where
  substVWith w i e (Core.VStar n a) =
    Core.VStar n <$> substVWith w i e a
  substVWith w i e (Core.VPrimTy p _) =
    -- TODO what about the annotation?
    substValueTypeWith w i e p
  substVWith w i e (Core.VPi π s t a) =
    Core.VPi π <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (Core.VLam t a) =
    Core.VLam <$> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (Core.VSig π s t a) =
    Core.VSig π <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (Core.VPair s t a) =
    Core.VPair <$> substVWith w i e s
      <*> substVWith w i e t
      <*> substVWith w i e a
  substVWith w i e (Core.VCatProduct s t a) =
    Core.VCatProduct <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (Core.VCatCoproduct s t a) =
    Core.VCatCoproduct <$> substVWith w i e s
      <*> substVWith (succ w) (succ i) e t
      <*> substVWith w i e a
  substVWith w i e (Core.VCatProductIntro s t a) =
    Core.VCatProductIntro <$> substVWith w i e s
      <*> substVWith w i e t
      <*> substVWith w i e a
  substVWith w i e (Core.VCatProductElimLeft t s a) =
    Core.VCatProductElimLeft
      <$> substVWith w i e t
      <*> substVWith w i e s
      <*> substVWith w i e a
  substVWith w i e (Core.VCatProductElimRight t s a) =
    Core.VCatProductElimRight
      <$> substVWith w i e t
      <*> substVWith w i e s
      <*> substVWith w i e a
  substVWith w i e (Core.VCatCoproductIntroLeft s a) =
    Core.VCatCoproductIntroLeft <$> substVWith w i e s
      <*> substVWith w i e a
  substVWith w i e (Core.VCatCoproductIntroRight s a) =
    Core.VCatCoproductIntroRight <$> substVWith w i e s
      <*> substVWith w i e a
  substVWith w i e (Core.VCatCoproductElim t1 t2 cp s t a) =
    Core.VCatCoproductElim
      <$> substVWith w i e cp
      <*> substVWith w i e t1
      <*> substVWith w i e t2
      <*> substVWith w i e s
      <*> substVWith w i e t
      <*> substVWith w i e a
  substVWith w i e (Core.VUnitTy a) =
    Core.VUnitTy <$> substVWith w i e a
  substVWith w i e (Core.VUnit a) =
    Core.VUnit <$> substVWith w i e a
  substVWith w i e (Core.VRecordTy flds a) =
    Core.VRecordTy <$> substTele w i e flds <*> substVWith w i e a
  substVWith w i e (Core.VRecord flds a) =
    Core.VRecord <$> traverse (substVWith w i e) flds <*> substVWith w i e a
  substVWith w i e (Core.VNeutral n a) =
    substNeutralWith w i e n a
  substVWith w i e (Core.VPrim p _) =
    -- TODO what about the annotation?
    substValueWith w i e p
  substVWith w i e (Core.ValueX a) =
    Core.ValueX <$> substVWith w i e a

substTele ::
  (Comonad w, HasSubstV extV primTy primVal a) =>
  Natural ->
  Core.BoundVar ->
  Core.Value extV primTy primVal ->
  [w a] ->
  Either (ErrorValue extV primTy primVal) [w a]
substTele _ _ _ [] = pure []
substTele w i e (f : fs) = do
  t'  <- substVWith w i e $ extract f
  fs' <- substTele (succ w) (succ i) e fs
  pure $ (t' <$ f) : fs'


-- | Perform substitution on a 'IR.Neutral'.
--
-- In general this might create any kind of value, which is why this can't be
-- made into a 'HasSubstV' instance. In the case that it is, then the outermost
-- 'IR.VNeutral' constructor might need an annotation, which is what the final
-- argument is for. In many cases there is no actual annotation so @()@ will do.
substNeutralWith ::
  ( AllSubstV extV primTy primVal,
    Monoid (Core.XVNeutral extV primTy primVal),
    Monoid (Core.XVLam extV primTy primVal),
    Monoid (Core.XVPrimTy extV primTy primVal),
    Monoid (Core.XVPrim extV primTy primVal),
    Show primTy,
    Show primVal
  ) =>
  -- | How many bindings have been traversed so far.
  Natural ->
  -- | Variable to substitute.
  Core.BoundVar ->
  -- | Value to substitute with.
  Core.Value extV primTy primVal ->
  -- | Neutral to perform substitution on.
  Core.Neutral extV primTy primVal ->
  -- | Annotation to use if the result is still neutral.
  Core.XVNeutral extV primTy primVal ->
  Either (ErrorValue extV primTy primVal) (Core.Value extV primTy primVal)
substNeutralWith w i e (Core.NBound j a) b = do
  a' <- substVWith w i e a
  b' <- substVWith w i e b
  pure $ case compare j i of
    LT -> Core.VNeutral (Core.NBound j a') b'
    EQ -> weakBy w e
    GT -> Core.VNeutral (Core.NBound (pred j) a') b'
substNeutralWith w i e (Core.NFree x a) b =
  Core.VNeutral <$> (Core.NFree x <$> substVWith w i e a)
    <*> substVWith w i e b
substNeutralWith w i e (Core.NApp f s a) _ =
  join $ vapp
    <$> substVWith w i e a
    <*> substNeutralWith w i e f mempty
    <*> substVWith w i e s
substNeutralWith w i e (Core.NRecElim ns f s t a) _ =
  join $ vrecelim
    <$> substVWith w i e a
    <*> pure ns
    <*> substNeutralWith w i e f mempty
    <*> substVWith (succ w) (succ i) e s
    <*> substVWith (len + w) (len + i) e t
  where len = lengthN ns
substNeutralWith w i e (Core.NeutralX a) b =
  Core.VNeutral <$> (Core.NeutralX <$> substVWith w i e a)
    <*> substVWith w i e b

-- | Perform a record elimination.
vrecelim ::
  forall extV primTy primVal.
  ( AllSubstV extV primTy primVal,
    Monoid (Core.XVNeutral extV primTy primVal),
    Monoid (Core.XVLam extV primTy primVal),
    Monoid (Core.XVPrimTy extV primTy primVal),
    Monoid (Core.XVPrim extV primTy primVal),
    Show primVal,
    Show primTy
  ) =>
  -- | the annotation to use if the result is another application node
  -- (if it isn't, then this annotation is unused)
  Core.XNRecElim extV primTy primVal ->
  -- | List of field names.
  [Symbol] ->
  -- | Record being eliminated.
  Core.Value extV primTy primVal ->
  -- | Return type.
  Core.Value extV primTy primVal ->
  -- | Body.
  Core.Value extV primTy primVal ->
  Either (ErrorValue extV primTy primVal) (Core.Value extV primTy primVal)
vrecelim a ns f s t = case f of
  Core.VNeutral n _ ->
    pure $ Core.VNeutral (Core.NRecElim ns n s t a) mempty
  Core.VRecord flds _
    | HashSet.fromList ns == HashMap.keysSet flds ->
        foldrM (\x -> substV $ flds!x) t ns
        -- [todo] simultaneous substitution
  _ ->
    Left $ ExpectedRecord {fields = ns, val = f}

-- | Apply two values.
vapp ::
  forall extV primTy primVal.
  ( AllSubstV extV primTy primVal,
    Monoid (Core.XVNeutral extV primTy primVal),
    Monoid (Core.XVLam extV primTy primVal),
    Monoid (Core.XVPrimTy extV primTy primVal),
    Monoid (Core.XVPrim extV primTy primVal),
    Show primVal,
    Show primTy
  ) =>
  -- | the annotation to use if the result is another application node
  -- (if it isn't, then this annotation is unused)
  Core.XNApp extV primTy primVal ->
  -- | Function value.
  Core.Value extV primTy primVal ->
  -- | Argument to the function.
  Core.Value extV primTy primVal ->
  Either (ErrorValue extV primTy primVal) (Core.Value extV primTy primVal)
vapp ann s t =
  case s of
    Core.VLam s _ -> substV t s
    Core.VNeutral f _ -> pure $ Core.VNeutral (Core.NApp f s ann) mempty
    Core.VPrimTy p _ -> case t of
      Core.VPrimTy q _ ->
        app' ApplyErrorT Core.VPrimTy (const Param.pureArg) p q
      Core.VNeutral (Core.NFree (Core.Global y) _) _ ->
        -- TODO pattern vars also
        app' ApplyErrorT Core.VPrimTy Param.freeArg p y
      Core.VNeutral (Core.NBound i _) _ ->
        app' ApplyErrorT Core.VPrimTy Param.boundArg p i
      _ ->
        Left $ CannotApply s t NoApplyError
    Core.VPrim p _ -> case t of
      Core.VPrim q _ ->
        app' ApplyErrorV Core.VPrim (const Param.pureArg) p q
      Core.VNeutral (Core.NFree (Core.Global y) _) _ ->
        -- TODO pattern vars also
        app' ApplyErrorV Core.VPrim Param.freeArg p y
      Core.VNeutral (Core.NBound i _) _ ->
        app' ApplyErrorV Core.VPrim Param.boundArg p i
      _ ->
        Left $ CannotApply s t NoApplyError
    _ ->
      Left $ CannotApply s t NoApplyError
  where
    app' ::
      forall ann arg fun.
      (Param.CanApply fun, Monoid ann, Show arg) =>
      (Param.ApplyError fun -> ApplyError primTy primVal) ->
      (fun -> ann -> Core.Value extV primTy primVal) ->
      (Proxy fun -> arg -> Maybe (Param.Arg fun)) ->
      fun ->
      arg ->
      Either (ErrorValue extV primTy primVal) (Core.Value extV primTy primVal)
    app' err con mkArg p y =
      case mkArg Proxy y of
        Nothing -> Left $ CannotApply s t NoApplyError
        Just y ->
          Param.apply1 p y |> bimap (CannotApply s t . err) (`con` mempty)

-- | Generic substitution for @f@.
class GHasWeak f => GHasSubstV extV primTy primVal f where
  gsubstVWith ::
    -- | How many bindings have been traversed so far.
    Natural ->
    -- | Variable to substitute.
    Core.BoundVar ->
    -- | Value to substitute with.
    Core.Value extV primTy primVal ->
    f t ->
    Either (ErrorValue extV primTy primVal) (f t)

instance GHasSubstV ext primTy primVal U1 where gsubstVWith _ _ _ U1 = pure U1

instance GHasSubstV ext primTy primVal V1 where
  gsubstVWith _ _ _ v = case v of

instance
  ( GHasSubstV ext primTy primVal f,
    GHasSubstV ext primTy primVal g
  ) =>
  GHasSubstV ext primTy primVal (f :*: g)
  where
  gsubstVWith b i e (x :*: y) =
    (:*:) <$> gsubstVWith b i e x
      <*> gsubstVWith b i e y

instance
  ( GHasSubstV ext primTy primVal f,
    GHasSubstV ext primTy primVal g
  ) =>
  GHasSubstV ext primTy primVal (f :+: g)
  where
  gsubstVWith b i e (L1 x) = L1 <$> gsubstVWith b i e x
  gsubstVWith b i e (R1 x) = R1 <$> gsubstVWith b i e x

instance
  GHasSubstV ext primTy primVal f =>
  GHasSubstV ext primTy primVal (M1 i t f)
  where
  gsubstVWith b i e (M1 x) = M1 <$> gsubstVWith b i e x

instance
  HasSubstV ext primTy primVal f =>
  GHasSubstV ext primTy primVal (K1 k f)
  where
  gsubstVWith b i e (K1 x) = K1 <$> substVWith b i e x

instance HasSubstV ext primTy primVal ()

instance HasSubstV ext primTy primVal Void

instance HasSubstV ext primTy primVal Natural where
  substVWith _ _ _ n = pure n

instance HasSubstV ext primTy primVal Usage.T where
  substVWith _ _ _ π = pure π

instance
  ( HasSubstV ext primTy primVal a,
    HasSubstV ext primTy primVal b
  ) =>
  HasSubstV ext primTy primVal (a, b)

instance
  ( HasSubstV ext primTy primVal a,
    HasSubstV ext primTy primVal b,
    HasSubstV ext primTy primVal c
  ) =>
  HasSubstV ext primTy primVal (a, b, c)

instance
  ( HasSubstV ext primTy primVal a,
    HasSubstV ext primTy primVal b
  ) =>
  HasSubstV ext primTy primVal (Either a b)

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal (Maybe a)

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal [a]

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal (NonEmpty a)

instance
  HasSubstV ext primTy primVal a =>
  HasSubstV ext primTy primVal (Param.PrimType a)

instance HasSubstV ext primTy primVal Symbol where
  substVWith _ _ _ x = pure x

-- TODO generalise @IR.T@
instance
  ( HasWeak primTy,
    HasWeak primVal,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Show primTy,
    Show primVal
  ) =>
  HasSubstValueType
    IR.T
    (Param.KindedType primTy)
    (Param.TypedPrim primTy primVal)
    (Param.KindedType primTy)
  where
  substValueTypeWith b i e (App.Cont {fun, args}) = do
    args <- traverse (substVWith b i e . argToValueType) args
    foldlM (vapp ()) (IR.VPrimTy $ App.takeToReturn fun) args
  substValueTypeWith _ _ _ ret@(App.Return {}) =
    pure $ IR.VPrimTy ret

-- TODO generalise @IR.T@
instance
  ( HasWeak primTy,
    HasWeak primVal,
    Param.CanPrimApply Param.Star primTy,
    Param.CanPrimApply primTy primVal,
    Show primTy,
    Show primVal
  ) =>
  HasSubstValue
    IR.T
    (Param.KindedType primTy)
    (Param.TypedPrim primTy primVal)
    (Param.TypedPrim primTy primVal)
  where
  substValueWith b i e (App.Cont {fun, args}) = do
    args <- traverse (substVWith b i e . argToValue) args
    foldlM (vapp ()) (IR.VPrim $ App.takeToReturn fun) args
  substValueWith _ _ _ ret@(App.Return {}) =
    pure $ IR.VPrim ret

-- | Transform an `App.Arg` into a `IR.Value`.
argToValueType ::
  App.Arg (Param.PrimType Param.Star) primTy ->
  Typed.ValueT IR.T primTy primVal
argToValueType = \case
  App.TermArg ret -> IR.VPrimTy ret
  App.BoundArg i -> Core.VBound i
  App.FreeArg x -> Core.VFree $ Core.Global x

-- | Transform an `App.Arg` into a `IR.Value`.
argToValue ::
  App.Arg (Param.PrimType primTy) primVal ->
  Typed.ValueT IR.T primTy primVal
argToValue = \case
  App.TermArg ret -> IR.VPrim ret
  App.BoundArg i -> Core.VBound i
  App.FreeArg x -> Core.VFree $ Core.Global x
