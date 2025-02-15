{-# LANGUAGE RecordWildCards #-}

module Juvix.Core.Pipeline.ToIR where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import Juvix.Core.Translate (hrToIR)
import qualified Juvix.Core.Translate as Translate
import Juvix.Library hiding (show)

hrToIRSig ::
  (Show ty, Show val) =>
  Core.Sig HR.T ty val ->
  Core.Sig IR.T ty val
hrToIRSig d@Core.DataSig {sigDataType} = d {Core.sigDataType = hrToIR sigDataType}
hrToIRSig c@Core.ConSig {sigConType} = c {Core.sigConType = hrToIR <$> sigConType}
hrToIRSig v@Core.ValSig {sigValType} = v {Core.sigValType = hrToIR sigValType}

-- hrToIRSig (SpecialSig s) = SpecialSig s

hrToIRSigs ::
  (Show ty, Show val) =>
  Core.Sigs HR.T ty val ->
  Core.Sigs IR.T ty val
hrToIRSigs sigs = hrToIRSig <$> sigs

hrToIRDef ::
  (Show ty, Show val) =>
  Core.RawGlobal HR.T ty val ->
  Core.PatternMap Core.GlobalName ->
  (Core.PatternMap Core.GlobalName, Core.RawGlobal IR.T ty val)
hrToIRDef global pats =
  let (def, env) = Translate.exec pats mempty (hrToIRRawGlobal global)
   in (Translate.patToSym env, def)

hrToIRDefs ::
  (Show ty, Show val) =>
  HM.HashMap Core.GlobalName (Core.RawGlobal HR.T ty val) ->
  (Core.PatternMap Core.GlobalName, HM.HashMap Core.GlobalName (Core.RawGlobal IR.T ty val))
hrToIRDefs = HM.foldlWithKey' f (mempty, mempty)
  where
    f (m, defs) globalName def =
      let (m', def') = hrToIRDef def m
       in (m', HM.insert globalName def' defs)

hrToIRRawGlobal ::
  (Show primTy, Show primVal) =>
  Core.RawGlobal HR.T primTy primVal ->
  Translate.M (Core.RawGlobal IR.T primTy primVal)
hrToIRRawGlobal (Core.RawGDatatype d) = Core.RawGDatatype <$> hrToIRRawDatatype d
hrToIRRawGlobal (Core.RawGDataCon d) = Core.RawGDataCon <$> hrToIRRawDataCon d
hrToIRRawGlobal (Core.RawGFunction f) = Core.RawGFunction <$> hrToIRRawFun f
hrToIRRawGlobal (Core.RawGAbstract Core.RawAbstract {rawAbsType, ..}) =
  pure $ Core.RawGAbstract Core.RawAbstract {rawAbsType = hrToIR rawAbsType, ..}

hrToIRRawDatatype ::
  (Show primTy, Show primVal) =>
  Core.RawDatatype HR.T primTy primVal ->
  Translate.M (Core.RawDatatype IR.T primTy primVal)
hrToIRRawDatatype Core.RawDatatype {rawDataArgs, rawDataCons, ..} = do
  cons <- traverse hrToIRRawDataCon rawDataCons
  pure
    Core.RawDatatype
      { rawDataArgs = hrToIRRawArg <$> rawDataArgs,
        rawDataCons = cons,
        ..
      }

hrToIRRawArg ::
  (Show primTy, Show primVal) =>
  Core.RawDataArg HR.T primTy primVal ->
  Core.RawDataArg IR.T primTy primVal
hrToIRRawArg Core.RawDataArg {rawArgType, ..} = Core.RawDataArg {rawArgType = hrToIR rawArgType, ..}

hrToIRRawDataCon ::
  (Show primTy, Show primVal) =>
  Core.RawDataCon HR.T primTy primVal ->
  Translate.M (Core.RawDataCon IR.T primTy primVal)
hrToIRRawDataCon Core.RawDataCon {rawConType, rawConDef, ..} = do
  mDef <- traverse hrToIRRawFun rawConDef
  pure $
    Core.RawDataCon
      { rawConType = hrToIR rawConType,
        rawConDef = mDef,
        ..
      }

hrToIRRawFun ::
  (Show primTy, Show primVal) =>
  Core.RawFunction HR.T primTy primVal ->
  Translate.M (Core.RawFunction IR.T primTy primVal)
hrToIRRawFun Core.RawFunction {rawFunType, rawFunClauses, ..} = do
  fclauses <- traverse hrToIRRawFunClause rawFunClauses
  pure $
    Core.RawFunction
      { rawFunType = hrToIR rawFunType,
        rawFunClauses = fclauses,
        ..
      }

hrToIRRawFunClause ::
  (Show primTy, Show primVal) =>
  Core.RawFunClause HR.T primTy primVal ->
  Translate.M (Core.RawFunClause IR.T primTy primVal)
hrToIRRawFunClause Core.RawFunClause {rawClauseTel, rawClausePats, rawClauseBody, ..} =
  do
    pats <- traverse Translate.hrPatternToIR' rawClausePats
    body <- Translate.hrToIR' rawClauseBody
    pure
      Core.RawFunClause
        { rawClauseTel = hrToIRRawTeleEle <$> rawClauseTel,
          rawClausePats = pats,
          rawClauseBody = body,
          ..
        }

hrToIRRawTeleEle ::
  forall primTy primVal.
  (HasCallStack, Show primTy, Show primVal) =>
  Core.RawTeleEle' HR.T primTy primVal ->
  Core.RawTeleEle' IR.T primTy primVal
hrToIRRawTeleEle Core.RawTeleEle {rawTy, rawExtension, ..} =
  Core.RawTeleEle
    { rawTy = hrToIR rawTy,
      rawExtension = notImplemented, -- TODO: TransformExt.Coerce rawExtension,
      ..
    }
