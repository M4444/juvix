{-# LANGUAGE UndecidableInstances #-}

module Juvix.Pipeline.ToHR.Sig where

import Control.Lens hiding ((|>))
import qualified Juvix.Context as Ctx
import qualified Juvix.Context.InfoNames as Info
import qualified Juvix.Context as Context
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR as HR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import Juvix.Pipeline.ToHR.Env
import Juvix.Pipeline.ToHR.Sig.Extract
import Juvix.Pipeline.ToHR.Term (transformTermHR)
import Juvix.Pipeline.ToHR.TypeSig
  ( transformTypeSig,
  )
import Juvix.Pipeline.ToHR.Types
import Juvix.Pipeline.ToHR.Usage
  ( transformGUsage,
    transformUsage,
  )
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure

transformSig ::
  ( HasPatVars m,
    HasThrowFF HR.T primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs HR.T primTy primVal m,
    HasClosure m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.T ->
  Ctx.Info ->
  m [CoreSig HR.T primTy primVal]
transformSig x def = trySpecial <||> tryNormal
  where
    q = NameSymbol.mod x
    trySpecial = fmap SpecialSig <$> transformSpecial q def
    tryNormal = transformNormalSig q x def
    x <||> y = x >>= maybe y (pure . pure)

extractDataConstructorSigs :: Sexp.T -> [Sexp.T]
extractDataConstructorSigs (typeCons Sexp.:> _ Sexp.:> dataCons)
  | Just dataConsL <- Sexp.toList dataCons =
    fmap
      (\n -> Sexp.cdr n Sexp.:> Sexp.car typeCons)
      dataConsL
extractDataConstructorSigs _t = []

transformNormalSig ::
  ( ReduceEff HR.T primTy primVal m,
    HasPatVars m,
    HasParam primTy primVal m,
    HasClosure m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Ctx.Info ->
  m [CoreSig HR.T primTy primVal]
transformNormalSig q x info =
  case info ^. Ctx.def of
    Ctx.Module moduleooo -> panic $ "Record not implemented" <> show moduleooo
    Ctx.CurrentNameSpace -> pure []
    Ctx.Term t
      | Structure.isType t -> transformTypeSig q x t
      | Structure.isSumCon t -> pure [CoreSig $ Core.ConSig {sigConType = Nothing}]
      | Structure.isSumConFilled t -> do
        let x' = conDefName x
            Just (Structure.SumConFilled _name def) = Structure.toSumConFilled t
            defMTy = Ctx.lookupInfoSexp info Info.signature
        defSigs <- transformNormalSig q x' (Ctx.Info mempty (Context.Term def))
        conSigs <- CoreSig . Core.ConSig <$> traverse (transformTermHR q) defMTy
        pure $
             conSigs
              : defSigs

      | Structure.isLambdaCase t ->
        Ctx.lookupInfoSexp info Info.signature
          |> transformValSig q x (info ^. Ctx.def) (Ctx.lookupInfo @Usage.T info Info.usage)
          >>| pure
      | otherwise -> pure []

transformValSig ::
  ( HasThrowFF HR.T primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs HR.T primTy primVal m,
    HasClosure m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Ctx.Definition ->
  Maybe Usage.T ->
  Maybe Sexp.T ->
  m (CoreSig HR.T primTy primVal)
transformValSig q _ _ _ (Just (Sexp.List [usage, usageExpr, arrow]))
  | Sexp.isAtomNamed usage ":usage" =
    CoreSig
      <$> (Core.ValSig <$> transformGUsage q (Just usageExpr) <*> transformTermHR q arrow)
transformValSig q _ _ _ (Just ty) =
  CoreSig <$> (Core.ValSig <$> transformGUsage q Nothing <*> transformTermHR q ty)
transformValSig _ x def _ _ = throwFF $ SigRequired x def

transformSpecial ::
  ( Show primTy,
    Show primVal,
    HasThrowFF ext primTy primVal m,
    HasCoreSigs ext primTy primVal m,
    HasClosure m
  ) =>
  NameSymbol.Mod ->
  Ctx.Info ->
  m (Maybe Special)
transformSpecial q info@(Ctx.Info _table def@(Ctx.Term term))
  | Just (Structure.LambdaCase [Structure.ArgBody Sexp.Nil rhs]) <-
      Structure.toLambdaCase term = do
    rhs <- transformSpecialRhs q rhs
    when (isJust rhs) do
      unless (isNothing (Ctx.lookupInfoSexp info Info.usage)) $
        throwFF $ BuiltinWithUsage def
      unless (isNothing (Ctx.lookupInfoSexp info Info.signature)) $
        throwFF $ BuiltinWithTypeSig def
    pure rhs
transformSpecial _ _ = pure Nothing

transformSpecialRhs ::
  ( Show primTy,
    Show primVal,
    HasThrowFF ext primTy primVal m,
    HasCoreSigs ext primTy primVal m,
    HasClosure m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (Maybe Special)
transformSpecialRhs _ (Sexp.List [name, prim])
  | Sexp.isAtomNamed name ":primitive",
    Just Sexp.A {atomName} <- Sexp.atomFromT prim =
    case atomName of
      "Builtin" :| ["Arrow"] -> pure $ Just $ ArrowS Nothing
      "Builtin" :| ["Pair"] -> pure $ Just $ PairS Nothing
      "Builtin" :| ["SAny"] -> pure $ Just SAnyS
      "Builtin" :| ["Colon"] -> pure $ Just ColonS
      "Builtin" :| ["Type"] -> pure $ Just TypeS
      "Builtin" :| ["CatProduct"] -> pure $ Just $ CatProductS
      "Builtin" :| ["CatCoproduct"] -> pure $ Just $ CatCoproductS
      "Builtin" :| ["CatProductIntro"] -> pure $ Just $ CatProductIntroS
      "Builtin" :| ["CatProductElimLeft"] -> pure $ Just $ CatProductElimLeftS
      "Builtin" :| ["CatProductElimRight"] -> pure $ Just $ CatProductElimRightS
      "Builtin" :| ["CatCoproductIntroLeft"] -> pure $ Just $ CatCoproductIntroLeftS
      "Builtin" :| ["CatCoproductIntroRight"] -> pure $ Just $ CatCoproductIntroRightS
      "Builtin" :| ["CatCoproductElim"] -> pure $ Just $ CatCoproductElimS
      "Builtin" :| (s : ss) -> throwFF $ UnknownBuiltin $ s :| ss
      _ -> pure Nothing
transformSpecialRhs q prim
  | Just a@Sexp.A {} <- Sexp.atomFromT prim =
    getSpecialSig q (Sexp.Atom a)
transformSpecialRhs q (Sexp.List [f, arg])
  | Just Sexp.A {atomName} <- Sexp.atomFromT f =
    case show atomName of
      ':' : _ -> pure Nothing
      _ -> do
        head <- getSpecialSig q f
        case head of
          Just (ArrowS Nothing) -> Just . ArrowS . Just <$> transformUsage q arg
          Just (PairS Nothing) -> Just . PairS . Just <$> transformUsage q arg
          Just CatProductS -> pure $ Just $ CatProductS
          Just CatCoproductS -> pure $ Just $ CatCoproductS
          Just CatProductIntroS -> pure $ Just $ CatProductIntroS
          Just CatProductElimLeftS -> pure $ Just $ CatProductElimLeftS
          Just CatProductElimRightS -> pure $ Just $ CatProductElimRightS
          Just CatCoproductIntroLeftS -> pure $ Just $ CatCoproductIntroLeftS
          Just CatCoproductIntroRightS -> pure $ Just $ CatCoproductIntroRightS
          Just CatCoproductElimS -> pure $ Just $ CatCoproductElimS
          _ -> pure Nothing
transformSpecialRhs _ _ = pure Nothing

conDefName :: NameSymbol.T -> NameSymbol.T
conDefName = identity -- NameSymbol.applyBase (<> "$def")

eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing

-- | If two signatures can be merged (currently, only constructor signatures),
-- then do so, otherwise return the *first* unchanged
-- (since @insertWith@ calls it as @mergeSigs new old@).
mergeSigs ::
  CoreSig ext primTy primVal ->
  CoreSig ext primTy primVal ->
  CoreSig ext primTy primVal
mergeSigs (CoreSig (Core.ConSig newTy)) (CoreSig (Core.ConSig oldTy)) =
  CoreSig $ Core.ConSig (newTy <|> oldTy)
mergeSigs _ second = second
