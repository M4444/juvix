{-# LANGUAGE ViewPatterns #-}

module Juvix.Pipeline.ToHR
  ( contextToHR,
    -- we export these functions to be able to call them stepwise from
    -- a testing place
    addSig,
    addDef,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Juvix.Closure as Closure
import qualified Juvix.Context as Context
import qualified Juvix.Context.InfoNames as Info
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Context.Traversal as Context
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Common.Context.Traverse as ContextT
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Pipeline.ToHR.Def as Def
import qualified Juvix.Pipeline.ToHR.Env as Env
import qualified Juvix.Pipeline.ToHR.Sig as Sig
import qualified Juvix.Pipeline.ToHR.Types as Types
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure
import qualified Prelude (error)

contextToHR ::
  (Show primTy, Show primVal) =>
  Context.T ->
  P.Parameterisation primTy primVal ->
  Either (Types.Error HR.T primTy primVal) (Core.RawGlobals HR.T primTy primVal)
contextToHR ctx param =
  Env.evalEnvEither ctx param do
    newCtx <- Context.traverseContext ctx attachConstructor

    let ordered = ContextT.recGroups newCtx

    for_ ordered \grp -> do
      traverse_ addSig grp

    for_ ordered \grp -> do
      traverse_ addDef grp
    >>| HM.mapMaybe Types.toCoreDef . Env.coreDefs
  where
    -- TODO
    -- put @"ffOrder" ordered

    -- Attaches the sum constructor with a data constructor filling
    attachConstructor ::
      (HasState "closure" Closure.T m) =>
      Context.Input Context.Info ->
      m (Context.Additional Context.Info)
    attachConstructor
      Context.Input {info = info@(Context.Info tb tm), name, currentContext} =
        case tm of
          Context.Term tm
            | Just (Structure.SumCon sumTName) <- Structure.toSumCon tm -> do
              let dataCons = NameSpace.extractValue name
              --
              modify @"closure" $ Closure.insertGeneric (NameSymbol.toSymbol sumTName)
              --
              let dataConstructor = Sexp.atom $ NameSymbol.fromSymbol dataCons
                  typeCons = Sexp.atom $ sumTName
                  -- figure out the type it refers to, and if it's there,
                  -- start injecting information.
                  infoDef = do
                    t <-
                      extractTypeDeclar . Context.infoDef . Context.extractValue
                        =<< Context.lookup sumTName currentContext
                    declaration <- Sexp.findKey Sexp.car dataConstructor t
                    tb
                      |> HashMap.insert Info.signature (generateSumConsSexp typeCons declaration)
                      |> HashMap.insert Info.usage (Sexp.serialize Usage.SAny)
                      |> Just
                  --
                  term =
                    case infoDef of
                      Just updatedTable ->
                        Structure.SumConFilled
                          sumTName
                          (Sexp.list [Sexp.atom ":primitive", Sexp.atom "Builtin.Constructor"])
                          |> Structure.fromSumConFilled
                          |> Context.Term
                          |> Context.Info updatedTable
                      Nothing ->
                        Structure.SumCon sumTName
                          |> Structure.fromSumCon
                          |> Context.Term
                          |> Context.Info tb
              pure $ (Context.Additional term [])
            | otherwise -> pure (Context.Additional info [])
          Context.CurrentNameSpace -> pure (Context.Additional info [])
          Context.Module _________ -> pure (Context.Additional info [])

    generateSumConsSexp typeCons (Sexp.cdr -> declaration) = do
      sanitizeRecord $ Sexp.foldr f typeCons declaration
      where
        sanitizeRecord (x Sexp.:> fields)
          | Sexp.isAtomNamed x ":record-d" = Sexp.list $ removeFieldNames fields
        sanitizeRecord xs = xs
        removeFieldNames fields
          | Just l <- Sexp.toList (Sexp.groupBy2 fields) =
            Sexp.cdr <$> l
          | otherwise =
            Prelude.error "ToHR.removeFieldNames not defined on different fields"
        f n acc = Sexp.list [arrow, n, acc]
        arrow = Sexp.atom "TopLevel.Prelude.->"

addSig ::
  ( Show primTy,
    Show primVal,
    HasState "closure" Closure.T m,
    HasThrow "fromToHRError" (Types.Error HR.T primTy primVal) m,
    HasReader "param" (P.Parameterisation primTy primVal) m,
    HasState "coreSigs" (Types.CoreSigs HR.T primTy primVal) m,
    HasState "patVars" (HM.HashMap Core.GlobalName Core.PatternVar) m
  ) =>
  ContextT.Entry ->
  m ()
addSig (ContextT.Entry x feDef) = do
  sigs <- Sig.transformSig x feDef
  for_ sigs $ modify @"coreSigs" . HM.insertWith Sig.mergeSigs x

addDef ::
  ( Show primTy,
    Show primVal,
    HasState "closure" Closure.T m,
    HasThrow "fromToHRError" (Types.Error HR.T primTy primVal) m,
    HasReader "param" (P.Parameterisation primTy primVal) m,
    HasState "coreDefs" (Types.CoreDefs HR.T primTy primVal) m,
    HasState "coreSigs" (Types.CoreSigs HR.T primTy primVal) m,
    HasState "nextPatVar" Core.PatternVar m,
    HasState "patVars" (HM.HashMap Core.GlobalName Core.PatternVar) m
  ) =>
  ContextT.Entry ->
  m ()
addDef (ContextT.Entry x feDef) = do
  defs <- Def.transformDef x feDef
  for_ defs \def -> do
    modify @"coreDefs" $ HM.insert (Def.defName def) def

extractTypeDeclar :: Context.Definition -> Maybe Sexp.T
extractTypeDeclar (Context.Term t)
  | Structure.isType t = Just t
extractTypeDeclar _ = Nothing
