{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.BerlinPipeline.Automation where

import Control.Lens as Lens hiding ((|>))
import qualified Control.Lens.TH as TH
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------

data T
  = ProcessJob ProcessJobNoEnv
  | UpdateJob
      { newContext :: Context.T Sexp.T Sexp.T Sexp.T,
        process :: ProcessJob
      }
  deriving (Show)

type Job = T

class HasExtract a m | a -> m where
  extract :: a x -> m (Pipeline.COut x)

----------------------------------------
-- Output Types
----------------------------------------

data Stage
  = Current
  | FromTopToCurrent
  | Eval
  deriving (Show, Eq)

data ProcessJob = Process
  { _current :: Pipeline.EnvOrSexp,
    _newForms :: [(Stage, Pipeline.EnvOrSexp)]
  }
  deriving (Show)

data ProcessJobNoEnv = ProcessNoEnv
  { _current :: Sexp.T,
    _newForms :: [(Stage, Sexp.T)]
  }
  deriving (Show)

TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''ProcessJob
TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''ProcessJobNoEnv

promoteSimpleForms :: [(Stage, Sexp.T)] -> [(Stage, Pipeline.EnvOrSexp)]
promoteSimpleForms = fmap (second Pipeline.Sexp)

promoteNoEnvToEnv :: ProcessJobNoEnv -> ProcessJob
promoteNoEnvToEnv process =
  Process
    { _newForms = promoteSimpleForms (process ^. newForms),
      _current = Pipeline.Sexp (process ^. current)
    }

----------------------------------------
-- Input Types
----------------------------------------

data PassArgument = PassArgument
  { _current :: Pipeline.EnvOrSexp,
    _context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

data SimplifiedPassArgument = SimplifiedArgument
  { _current :: Sexp.T,
    _context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''PassArgument
TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''SimplifiedPassArgument

applySimplifiedPass ::
  Meta.HasMeta m =>
  (PassArgument -> m Job) ->
  Pipeline.CIn ->
  m Pipeline.WorkingEnv
applySimplifiedPass f Pipeline.CIn {languageData, surroundingData} =
  let Pipeline.WorkingEnv {currentExp, context} = languageData
      Pipeline.SurroundingEnv {metaInfo} = surroundingData
      initialOutput = Pipeline.WorkingEnv {currentExp = [], context = context}
   in Meta.put metaInfo >> foldM g initialOutput currentExp
  where
    g Pipeline.WorkingEnv {context, currentExp} nextSexp = do
      job <- f PassArgument {_current = nextSexp, _context = context}
      (sexp, newContext, newForms) <- extractFromJob context job
      Pipeline.WorkingEnv
        { context = newContext,
          -- TODO: Only Stage = Current is supported, add handling for other cases.
          currentExp = currentExp <> [Pipeline.Sexp sexp] <> map snd newForms
        }
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
      case context Context.!? name of
        Just def -> do
          -- Abstract this out, improve the interface to the
          -- Context... why is it this complicated?
          let (ourDef, resolvedName) = Context.resolveName context (def, name)
          case ourDef of
            Context.Def d -> updateDef d resolvedName Context.Def
            Context.TypeDeclar typ ->
              updateTerms
                name
                (\[s] -> Context.TypeDeclar s)
                context
                jobViaSimplified
                [typ]
            Context.SumCon s@Context.Sum {sumTDef} -> do
              case sumTDef of
                Nothing -> noOpJob |> pure
                Just d ->
                  updateDef
                    d
                    resolvedName
                    ( \newDef ->
                        s {Context.sumTDef = Just newDef}
                          |> Context.SumCon
                    )
            Context.Record r@Context.Rec {recordMTy} -> do
              case recordMTy of
                Nothing -> noOpJob |> pure
                Just recordMTy ->
                  updateTerms
                    name
                    ( \[newMTy] ->
                        r {Context.recordMTy = Just newMTy}
                          |> Context.Record
                    )
                    context
                    jobViaSimplified
                    [recordMTy]
            u@Context.Unknown {definitionMTy} -> do
              case definitionMTy of
                Nothing -> noOpJob |> pure
                Just definitionMTy ->
                  updateTerms
                    name
                    ( \[newMTy] ->
                        u {Context.definitionMTy = Just newMTy}
                    )
                    context
                    jobViaSimplified
                    [definitionMTy]
            _ -> noOpJob |> pure
        Nothing ->
          throw @"error" $
            Sexp.string
              "Could not find definition when \
              \ it was promised to be in the environment"
  where
    simplified context sexp = (SimplifiedArgument {_current = sexp, _context = context})

    jobViaSimplified context sexp = simplified context sexp |> f

    updateDef d@Context.D {defTerm, defMTy = Nothing} name constructor =
      updateTerms
        name
        (\[defTerm] -> constructor d {Context.defTerm = defTerm})
        context
        jobViaSimplified
        [defTerm]
    updateDef d@Context.D {defTerm, defMTy = Just mTy} name constructor =
      updateTerms
        name
        (\[defTerm, defMTy] -> constructor d {Context.defTerm = defTerm, Context.defMTy = Just defMTy})
        context
        jobViaSimplified
        [defTerm, mTy]

    noOpJob = UpdateJob context Process {_current = current, _newForms = []}

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
