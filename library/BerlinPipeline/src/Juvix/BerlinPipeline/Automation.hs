{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.BerlinPipeline.Automation where

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
  { current :: Pipeline.EnvOrSexp,
    newForms :: [(Stage, Pipeline.EnvOrSexp)]
  }
  deriving (Show)

data ProcessJobNoEnv = ProcessNoEnv
  { current :: Sexp.T,
    newForms :: [(Stage, Sexp.T)]
  }
  deriving (Show)

promoteSimpleForms :: [(Stage, Sexp.T)] -> [(Stage, Pipeline.EnvOrSexp)]
promoteSimpleForms = fmap (second Pipeline.Sexp)

promoteNoEnvToEnv :: ProcessJobNoEnv -> ProcessJob
promoteNoEnvToEnv (ProcessNoEnv {current, newForms}) =
  Process
    { newForms = promoteSimpleForms newForms,
      current = Pipeline.Sexp current
    }

----------------------------------------
-- Input Types
----------------------------------------

data PassArgument = PassArgument
  { current :: Pipeline.EnvOrSexp,
    context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

data SimplifiedPassArgument = SimplifiedArgument
  { current :: Sexp.T,
    context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

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
      job <- f PassArgument {current = nextSexp, context = context}
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
simplify f PassArgument {current, context} =
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
            -- TODO: Handle remaining cases that have Sexps
            _ -> noOpJob |> pure
        Nothing ->
          throw @"error"
            "Could not find definition when \
            \ it was promised to be in the environment"
  where
    simplified context sexp = (SimplifiedArgument {current = sexp, context})

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

    noOpJob = UpdateJob context Process {current = current, newForms = []}

-- | @extractFromJob@ extracts the Sexp that a Job processed, the context after
-- the Job was run and any new forms that the Job introduced.
extractFromJob ::
  HasThrow "error" Text f =>
  Context.T Sexp.T Sexp.T Sexp.T ->
  Job ->
  f
    ( Sexp.T,
      Context.T Sexp.T Sexp.T Sexp.T,
      [(Stage, Pipeline.EnvOrSexp)]
    )
extractFromJob context job =
  case job of
    ProcessJob ProcessNoEnv {current, newForms} ->
      (current, context, promoteSimpleForms newForms) |> pure
    UpdateJob {newContext, process = Process {current, newForms}} ->
      case current of
        Pipeline.Sexp sexp ->
          (sexp, newContext, newForms) |> pure
        Pipeline.InContext name ->
          throw @"error" $
            "Attempting to redefine term already in env" <> NameSymbol.toText name

-- | @updateTerms@ Processes a list of sexps in order and repackages the resulting
-- sexps into a Context.Definition.
updateTerms ::
  HasThrow "error" Text f =>
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
          { current = Pipeline.InContext name,
            newForms
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
