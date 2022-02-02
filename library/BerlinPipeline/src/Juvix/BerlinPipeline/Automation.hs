{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.BerlinPipeline.Automation where

import Control.Lens as Lens hiding ((|>))
import qualified Control.Lens.TH as TH
import Juvix.BerlinPipeline.Lens
import qualified Juvix.BerlinPipeline.Meta as Meta
--
-- Automation Specific types imported here
--
import Juvix.BerlinPipeline.Pipeline
  ( Job (..),
    PassArgument (..),
    ProcessJob (..),
    ProcessJobNoEnv (..),
    SimplifiedPassArgument (..),
    Stage (..),
  )
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------

type T = Job

-- Automation Specific types have been moved to Pipeline.hs

--------------------------------------------------------------------------------
-- Type Operations
--------------------------------------------------------------------------------

----------------------------------------
-- Output Types Operations
----------------------------------------

promoteSimpleForms :: [(Stage, Sexp.T)] -> [(Stage, Pipeline.EnvOrSexp)]
promoteSimpleForms = fmap (second Pipeline.Sexp)

promoteNoEnvToEnv :: ProcessJobNoEnv -> ProcessJob
promoteNoEnvToEnv process =
  Process
    { _newForms = promoteSimpleForms (process ^. newForms),
      _current = Pipeline.Sexp (process ^. current)
    }

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

applySimplifiedPass ::
  Meta.HasMeta m =>
  (PassArgument -> m Job) ->
  Pipeline.CIn ->
  m Pipeline.WorkingEnv
applySimplifiedPass f input =
  let workingEnv = input ^. languageData
      metaInform = input ^. surroundingData . metaInfo
      initialOut = set currentExp [] workingEnv
   in Meta.put metaInform
        >> foldM g initialOut (workingEnv ^. currentExp)
  where
    g workingEnv@Pipeline.WorkingEnv {_context} nextSexp = do
      job <- f PassArgument {_current = nextSexp, _context = _context}
      (sexp, newContext, newForms) <- extractFromJob _context job
      workingEnv
        |> set context newContext
        |> over currentExp (<> [Pipeline.Sexp sexp] <> map snd newForms)
        |> pure

runSimplifiedPass ::
  (Pipeline.HasExtract m, Meta.HasMeta m) =>
  (PassArgument -> m Job) ->
  Pipeline.CIn ->
  IO (Pipeline.COut Pipeline.WorkingEnv)
runSimplifiedPass f =
  Pipeline.extract . applySimplifiedPass f

simplify ::
  Meta.HasMeta m => (SimplifiedPassArgument -> m Job) -> PassArgument -> m Job
simplify f PassArgument {_current = current, _context = context} =
  case current of
    Pipeline.Sexp sexp ->
      f (simplified context sexp)
    Pipeline.InContext name -> do
      -- Wrappers over updating the consturctor (lenses by hand)
      let formDeclaration [s] = Context.TypeDeclar s
          formUnknown u [mTy] = u {Context.definitionMTy = Just mTy}
          formRecord r [maTy] = r {Context.recordMTy = Just maTy} |> Context.Record
          formSumCon s newDef = s {Context.sumTDef = Just newDef} |> Context.SumCon
      case context Context.!? name of
        Just def -> do
          -- Abstract this out, improve the interface to the
          -- Context... why is it this complicated?
          let (ourDef, resolvedName) = Context.resolveName context (def, name)
          case ourDef of
            Context.Def d ->
              updateDef d resolvedName Context.Def
            Context.SumCon s@Context.Sum {sumTDef = Just d} ->
              updateDef d resolvedName (formSumCon s)
            Context.TypeDeclar typ ->
              updateTerms name formDeclaration context jobViaSimplified [typ]
            Context.Record r@Context.Rec {recordMTy = Just recordMTy} ->
              updateTerms name (formRecord r) context jobViaSimplified [recordMTy]
            u@Context.Unknown {definitionMTy = Just definitionMTy} -> do
              updateTerms name (formUnknown u) context jobViaSimplified [definitionMTy]
            _ -> noOpJob context current |> pure
        Nothing ->
          throw @"error" $
            Sexp.string
              "Could not find definition when \
              \ it was promised to be in the environment"
  where
    simplified context sexp = SimplifiedArgument {_current = sexp, _context = context}

    jobViaSimplified context sexp = simplified context sexp |> f

    updateDef d@Context.D {defTerm, defMTy} name constructor =
      let formDefn [defTerm] =
            constructor d {Context.defTerm = defTerm}
          formDefn [defTerm, defMTy] =
            constructor d {Context.defTerm = defTerm, Context.defMTy = Just defMTy}
          arguments =
            maybe [defTerm] (\mty -> [defTerm, mty]) defMTy
       in updateTerms name formDefn context jobViaSimplified arguments

-- | @noOpJob@ is a Job that does nothing to the passed context and current form.
noOpJob ::
  Context.T Sexp.T Sexp.T Sexp.T -> Pipeline.EnvOrSexp -> Job
noOpJob context current = UpdateJob context Process {_current = current, _newForms = []}

-- | @extractFromJob@ extracts the Sexp that a Job processed, the context after
-- the Job was run and any new forms that the Job introduced.
extractFromJob ::
  HasThrow "error" Sexp.T f =>
  Context.T Sexp.T Sexp.T Sexp.T ->
  Job ->
  f
    ( Sexp.T,
      Context.T Sexp.T Sexp.T Sexp.T,
      [(Stage, Pipeline.EnvOrSexp)]
    )
extractFromJob context job =
  case job of
    ProcessJob process ->
      (process ^. current, context, promoteSimpleForms (process ^. newForms))
        |> pure
    UpdateJob {newContext, process} ->
      case process ^. current of
        Pipeline.Sexp sexp ->
          (sexp, newContext, process ^. newForms) |> pure
        Pipeline.InContext name ->
          ("Attempting to redefine term already in env" <> NameSymbol.toText name)
            |> Sexp.string
            |> throw @"error"

-- | @updateTerms@ Processes a list of sexps in order and repackages the resulting
-- sexps into a Context.Definition.
updateTerms ::
  HasThrow "error" Sexp.T f =>
  -- | The name of the symbol being updated
  NameSymbol.T ->
  -- | A function to repackage the processed sexps back into a Definition
  ([Sexp.T] -> Context.Definition Sexp.T Sexp.T Sexp.T) ->
  -- | The starting context
  Context.T Sexp.T Sexp.T Sexp.T ->
  -- | A function to process an sexp
  (Context.T Sexp.T Sexp.T Sexp.T -> Sexp.T -> f Job) ->
  -- | The list of Sexps to process
  [Sexp.T] ->
  f Job
updateTerms name rePackageTerm context toJob sexpsToProcess = do
  (context, sexps, newForms) <- foldM f (context, [], []) sexpsToProcess

  UpdateJob
    { newContext = Context.addGlobal name (rePackageTerm sexps) context,
      process =
        Process
          { _current = Pipeline.InContext name,
            _newForms = newForms
          }
    }
    |> pure
  where
    f (context, sexps, oldNewForms) sexpToUpdate = do
      job <- toJob context sexpToUpdate
      (sexp, context, newForms) <- extractFromJob context job
      pure (context, sexps <> [sexp], oldNewForms <> newForms)

-- | @extractProcessJob@ Will extract the promoted ProcessJob that
-- talks about the environment, even if the job is a @ProcessJob@ by
-- simply wrapping it.
extractProcessJob :: Job -> ProcessJob
extractProcessJob (UpdateJob {process}) = process
extractProcessJob (ProcessJob process) = promoteNoEnvToEnv process
