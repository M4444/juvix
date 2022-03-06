module Juvix.Pipeline.ToHR.TypeSig where

import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.HR as HR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import Juvix.Pipeline.ToHR.Env
import Juvix.Pipeline.ToHR.Term
import Juvix.Pipeline.ToHR.Types
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Parsing as Structure
import Prelude (error)
import qualified Juvix.Context as Context

transformTypeSig ::
  ( ReduceEff HR.T primTy primVal m,
    HasPatVars m,
    HasParam primTy primVal m,
    HasClosure m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Sexp.T ->
  m [CoreSig HR.T primTy primVal]
transformTypeSig q _name (_name2 Sexp.:> typeCon Sexp.:> args Sexp.:> typeForm)
  | Just typeArgs <- Sexp.toList args >>= traverse eleToSymbol = do
    (baseTy, hd) <- transformIndices typeArgs typeCon
    let sigDataType = foldr makeTPi baseTy typeArgs
    (sigDataCons, conSigs) <- unzip <$> transformConSigs q hd typeCon typeForm
    let dataSig = CoreSig (Core.DataSig {sigDataType, sigDataCons})
    pure $ dataSig : conSigs
  where
    transformIndices _ _ =
      pure (HR.Star $ Core.U 0, Just $ HR.Star $ Core.U 0) -- TODO metavar for universe
    makeTPi name res =
      -- TODO metavars for the named args instead of defaulting to types
      HR.Pi mempty name (HR.Star $ Core.U 0) res
transformTypeSig _ _ _ = error "malformed type"

transformConSigs ::
  ( HasClosure m,
    ReduceEff HR.T primTy primVal m,
    HasPatVars m,
    HasParam primTy primVal m,
    Show primTy,
    Show primVal
  ) =>
  -- | namespace containing declaration
  NameSymbol.Mod ->
  -- | datatype head
  Maybe (HR.Term primTy primVal) ->
  -- | type constructor
  Sexp.T ->
  -- | rhs
  Sexp.T ->
  m [(NameSymbol.T, CoreSig HR.T primTy primVal)]
transformConSigs pfx hd typeCon =
  traverse (transformProduct pfx hd typeCon) <=< toProducts
  where
    -- We have a single constructor, which is a record
    toProducts (record Sexp.:> Sexp.Nil)
      | Structure.isRecordDec record = do
        -- E.g.
        -- ((:record-d
        --      (x ω TopLevel.Prelude.Circuit.field)
        --      (y ω TopLevel.Prelude.Circuit.field)
        --      (z ω TopLevel.Prelude.Circuit.field)))
        --   ":record-d",
        --   ["Datatypes"],
        --   Nothing
        throwFF $ RecordUnimplemented record
    -- we can't have another standalone product here, so just send to
    -- sum
    toProducts sums
      | Just cons <- Sexp.toList sums = do
        pure $ map toProduct1 cons
    toProducts _ = error "malformed data constrcutor"
    toProduct1 (sumConstructor Sexp.:> value)
      | Just sumSym <- eleToSymbol sumConstructor =
        (sumSym, value)
    toProduct1 _ = error "malformed sum constructor"

transformProduct ::
  ( HasPatVars m,
    HasThrowFF HR.T primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs HR.T primTy primVal m,
    HasClosure m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.Mod ->
  -- | datatype head
  Maybe (Core.Term HR.T primTy primVal) ->
  -- | type constructor
  Sexp.T ->
  (NameSymbol.T, Sexp.T) ->
  m (NameSymbol.T, CoreSig HR.T primTy primVal)
transformProduct q hd typeCon (x, prod) =
  (x,) . makeSig
    <$> transformConSig q x hd typeCon prod
  where
    makeSig ty = CoreSig (Core.ConSig {sigConType = Just ty})

transformConSig ::
  (HasCallStack, ReduceEff HR.T primTy primVal m, HasPatVars m, Show primTy, Show primVal, HasClosure m) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  -- | datatype head
  Maybe (Core.Term HR.T primTy primVal) ->
  -- | type constructor
  Sexp.T ->
  Sexp.T ->
  m (Core.Term HR.T primTy primVal)
transformConSig q name mHd typeCon r@((t Sexp.:> ts) Sexp.:> _)
  | named ":record-d" = do
    let convertedSexp = Sexp.list [arrow, Sexp.list $ removeFieldNames ts, Sexp.car typeCon]
    transformConSig q name mHd typeCon convertedSexp
  -- throwFF $ RecordUnimplemented r

  | named ":arrow" = transformTermHR q ts
  | isNothing mHd = do
    transformTermHR q ts
  where
    arrow = Sexp.atom "TopLevel.Prelude.->"
    removeFieldNames fields
      | Just l <- Sexp.toList (Sexp.groupBy2 fields) = Sexp.cdr <$> l
      | otherwise = notImplemented

    -- g (Sexp.List [s, e]) = e
    named = Sexp.isAtomNamed t
-- Don't assume r is a cons, as an empty constructor has r = ()
transformConSig q name mHd _ r
  -- TODO: Should check if there any data constructor with a signature (See GADT)
  | isNothing mHd = do
    throwFF $ InvalidConstructor name r
  | Just hd <- mHd,
    Just xs <- Sexp.toList r =
    let makeArr (x, arg) res =
          HR.Pi (Usage.SNat 1) x <$> transformTermHR q arg <*> pure res
        names = makeFieldName <$> [0 ..]
        makeFieldName i = NameSymbol.fromText $ "$field" <> show (i :: Int)
     in foldrM makeArr hd $ zip names xs

eleToSymbol :: Sexp.T -> Maybe NameSymbol.T
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (Context.removeTopName atomName)
  | otherwise = Nothing
