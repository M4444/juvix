{-# LANGUAGE DuplicateRecordFields #-}

-- | the job of @Automation@ is to make the step function more
-- amenable to writing passes It is not ergonomic to take extra
-- information one may not care about
module Juvix.BerlinPipeline.Automation
  ( module Juvix.BerlinPipeline.Automation,
    module Juvix.BerlinPipeline.Pipeline,
  )
where

import Control.Lens as Lens hiding ((|>))
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
import qualified Juvix.Sexp.Structure.Berlin as Structure
import Juvix.Sexp.Structure.Lens hiding (fst, snd)

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------

-- | @T@ is the main type of the Automation Module, represents the
-- output type of an Automation pass.
type T = Job

-- Automation Specific types have been moved to Pipeline.hs

--------------------------------------------------------------------------------
-- Type Operations
--------------------------------------------------------------------------------

----------------------------------------
-- Output Types Operations
----------------------------------------

-- | @promoteSimpleForms@
promoteSimpleForms :: [(Stage, Sexp.T)] -> [(Stage, Pipeline.EnvOrSexp)]
promoteSimpleForms = fmap (second Pipeline.Sexp)

-- | @promoteNoEnvToEnv@ promotes a @ProcessJobNoEnv@ into a
-- @ProcessJob@
promoteNoEnvToEnv :: ProcessJobNoEnv -> ProcessJob
promoteNoEnvToEnv process =
  Process
    { _newForms = promoteSimpleForms (process ^. newForms),
      _current = Pipeline.Sexp (process ^. current)
    }

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

-- | @applySimplifiedPass@ serves as a HOF that allows for passes to
-- be a more simplified type, namely a function that takes a
-- @PassArgument@ to an effectual result over @Job@ that determines
-- how the pass should be brought together. Note that
--
-- @Meta.HasMega m => Pipeline.CIn -> Pipeline.WorkingEnv@
--
-- is an approximation of the @Step.T@ type without the Meta
-- information attached
applySimplifiedPass ::
  (MonadIO m, Meta.HasMeta m) =>
  (PassArgument -> m Job) ->
  Pipeline.CIn ->
  m Pipeline.WorkingEnv
applySimplifiedPass f input =
  let workingEnv = input ^. languageData
      metaInform = input ^. surroundingData . metaInfo
      initialOut = set currentExp [] workingEnv
   in Meta.put metaInform
        >> foldM runPassOnEnv initialOut (workingEnv ^. currentExp)
  where
    onStepPasses = input ^. surroundingData . onSinglePass
    beforSteps = filter ((== Pipeline.Before) . fst) onStepPasses |> fmap snd
    afterSteps = filter ((== Pipeline.After) . fst) onStepPasses |> fmap snd
    --
    runManyPasses passArg passes = do
      meta <- Meta.get
      value <-
        foldM (\pass fun -> fun pass) passArg passes
          |> (`Pipeline.extractAroundEnv` meta)
          |> liftIO
      case value of
        Left sexp -> throw @"error" sexp
        Right (pass, meta) -> pass <$ Meta.put meta
    --
    runPassOnEnv workingEnv@Pipeline.WorkingEnv {_context} nextSexp = do
      -- run before step passes
      -------------------------
      passArgumentWithBeforeStepsRan <-
        runManyPasses
          PassArgument {_current = nextSexp, _context = _context}
          beforSteps
      -- run the pass itself!
      -------------------------
      job <- f passArgumentWithBeforeStepsRan
      --
      let (sexp, newContext, newForms) = extractFromJobEnv _context job
      -- run the after step passes
      ----------------------------
      PassArgument {_current = sexp, _context = newContext} <-
        runManyPasses
          PassArgument {_current = sexp, _context = newContext}
          afterSteps
      -- Stitch back the output type
      ------------------------------
      workingEnv
        |> set context newContext
        |> over currentExp (<> [sexp] <> map snd newForms)
        |> pure

-- | @runSimplifiedPass@ simply combines the @Pipeline.extract@
-- function with @applySimplifiedPass@, to get the output effect,
-- which corresponds to a @Step.T@ with an effect attached to it
runSimplifiedPass ::
  (Pipeline.HasExtract m, Meta.HasMeta m, MonadIO m) =>
  (PassArgument -> m Job) ->
  Pipeline.CIn ->
  IO (Pipeline.COut Pipeline.WorkingEnv)
runSimplifiedPass f =
  Pipeline.extract . applySimplifiedPass f

-- | @simplify@ allows a pass to ignore the fact that expression
-- coming in may be added to the @Context.T@ already, and we can act
-- as if it were just a normal @Sexp.T@ being passed in.
simplify ::
  Meta.HasMeta m => (SimplifiedPassArgument -> m Job) -> PassArgument -> m Job
simplify f PassArgument {_current = current, _context = context} =
  case current of
    Pipeline.Sexp sexp ->
      f SimplifiedArgument {_current = sexp, _context = context}
    Pipeline.InContext name -> do
      case context Context.!? name of
        Nothing ->
          Sexp.string
            "Could not find definition when \
            \ it was promised to be in the environment"
            |> throw @"error"
        Just from -> do
          let combineF (ctx, newForms) sexp = do
                job <- f SimplifiedArgument {_context = ctx, _current = sexp}
                let (current, newCtx, newForms') = extractFromJobEnv ctx job
                    (toPutBack, relocated) =
                      case current of
                        Pipeline.Sexp sexp ->
                          (sexp, [])
                        Pipeline.InContext name ->
                          ( Structure.Relocated name |> Sexp.serialize,
                            [(Pipeline.Current, current)]
                          )
                --
                pure ((newCtx, newForms <> relocated <> newForms'), toPutBack)

              updateInformationTable =
                -- TODO ∷ Filter out Protected fields, so we don't run
                -- on those.
                traverseAccumM combineF (context, []) (info ^. Context.table)
              --
              info = Context.fromTerm from
              --
              setInfo infoTable = set Context.table infoTable
              setTerm termonolo = set Context.def termonolo
          case from ^. Context.term . Context.def of
            Context.Term tm -> do
              ((ctx, newForms), infoTable) <- updateInformationTable
              --
              job <- f SimplifiedArgument {_context = ctx, _current = tm}
              --
              (term, newCtx, newForms') <- extractFromJob ctx job
              --
              let newTerm :: Context.Info
                  newTerm = info |> setInfo infoTable |> setTerm (Context.Term term)
              --
              UpdateJob
                { newContext = Context.addGlobal name newTerm newCtx,
                  process =
                    Pipeline.Process {_current = current, _newForms = newForms <> newForms'}
                }
                |> pure
            Context.Module _mod -> do
              ((ctx, newForms), infoTable) <- updateInformationTable
              UpdateJob
                { newContext =
                    Context.addGlobal name (setInfo infoTable info) ctx,
                  process = Pipeline.Process {_current = current, _newForms = newForms}
                }
                |> pure
            Context.CurrentNameSpace ->
              noOpJob context current |> pure

-- | @noOpJob@ is a Job that does nothing to the passed context and current form.
noOpJob :: Context.T -> Pipeline.EnvOrSexp -> Job
noOpJob context current =
  UpdateJob context Process {_current = current, _newForms = []}

-- | @extractFromJob@ extracts the @EnvOrSexp@ that a Job processed,
-- the context after the Job was run and any new forms that the Job
-- introduced.
extractFromJobEnv ::
  Context.T ->
  Job ->
  (Pipeline.EnvOrSexp, Context.T, [(Stage, Pipeline.EnvOrSexp)])
extractFromJobEnv context job =
  case job of
    ProcessJob process ->
      let sexp = process ^. current |> Pipeline.Sexp
       in (sexp, context, promoteSimpleForms (process ^. newForms))
    UpdateJob {newContext, process} ->
      (process ^. current, newContext, process ^. newForms)

-- | @extractFromJob@ extracts the Sexp that a Job processed, the
-- context after the Job was run and any new forms that the Job
-- introduced.
extractFromJob ::
  HasThrow "error" Sexp.T f =>
  Context.T ->
  Job ->
  f (Sexp.T, Context.T, [(Stage, Pipeline.EnvOrSexp)])
extractFromJob context job =
  let contents = extractFromJobEnv context job
   in case contents of
        (Pipeline.Sexp sexp, ctx, newForms) ->
          (sexp, ctx, newForms) |> pure
        (Pipeline.InContext name, _, _) ->
          ("Attempting to redefine term already in env" <> NameSymbol.toText name)
            |> Sexp.string
            |> throw @"error"

-- | @extractProcessJob@ Will extract the promoted ProcessJob that
-- talks about the environment, even if the job is a @ProcessJob@ by
-- simply wrapping it.
extractProcessJob :: Job -> ProcessJob
extractProcessJob (UpdateJob {process}) = process
extractProcessJob (ProcessJob process) = promoteNoEnvToEnv process
