{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.BerlinPipeline.Env where

import Control.Lens as Lens hiding ((|>))
import qualified Data.HashSet as Set
import qualified Juvix.BerlinPipeline.CircularList as CircularList
import Juvix.BerlinPipeline.Lens
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.Step as Step
import qualified Juvix.Context as Context
import Juvix.Library hiding (empty)
import qualified Juvix.Library.NameSymbol as NameSymbol

--------------------------------------------------------------------------------
-- Type Specification
--------------------------------------------------------------------------------

-- TODO :: add a FullPipeline, that does not get consumed.
--
-- TODO :: rename to working environment, as this gets consumed
-- over time
data T = T
  { -- | @information@ serves as the public facing data to the
    -- environment that passes can access
    information :: Pipeline.CIn,
    --
    -- All other information is made to be exclusive with the Pipeline as
    -- a whole that the Steps do not bother with.
    -- This consists of information like how to deal with Traces between passes
    -- to the entire pass infrastructure itself.
    --
    --

    -- | @pipeline@ is the pipeline itself.
    -- It features all the steps named and registered with the system
    pipeline :: CircularList.T (Step.Named),
    -- | @stoppingStep@ determines if we are stopping the pipeline
    -- evaluation early. If the value is @Nothing@ then the @eval@
    -- function runs to the end.
    stoppingStep :: Maybe NameSymbol.T
  }
  deriving (Generic)

newtype EnvS b = EnvS (State T b)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "information" (Pipeline.CIn),
      HasSink "information" (Pipeline.CIn),
      HasSource "information" (Pipeline.CIn)
    )
    via StateField "information" (State T)
  deriving
    ( HasState "pipeline" (CircularList.T (Step.Named)),
      HasSink "pipeline" (CircularList.T (Step.Named)),
      HasSource "pipeline" (CircularList.T (Step.Named))
    )
    via StateField "pipeline" (State T)
  deriving
    ( HasState "stoppingStep" (Maybe NameSymbol.T),
      HasSink "stoppingStep" (Maybe NameSymbol.T),
      HasSource "stoppingStep" (Maybe NameSymbol.T)
    )
    via StateField "stoppingStep" (State T)

data StopADT = Stop

--------------------------------------------------------------------------------
-- Environment Functions
--------------------------------------------------------------------------------

-- | Registers a function that will run before each pass that takes an
-- argument
registerBeforeEachStep ::
  (Pipeline.PassArgument -> Pipeline.AroundMIO Pipeline.PassArgument) -> EnvS ()
registerBeforeEachStep fun = do
  modify
    @"information"
    (over (surroundingData . onSinglePass) ((Pipeline.Before, fun) :))

-- | Registers a function that will run after each pass that takes an
-- argument
registerAfterEachStep ::
  (Pipeline.PassArgument -> Pipeline.AroundMIO Pipeline.PassArgument) -> EnvS ()
registerAfterEachStep fun = do
  modify
    @"information"
    (over (surroundingData . onSinglePass) ((Pipeline.After, fun) :))

-- | Register the pipeline function to the environment
registerStep :: CircularList.T Step.Named -> EnvS ()
registerStep l = do
  modify @"pipeline" (<> l)

-- | Create a named group of pipeline steps or nested grouping of
-- pipeline steps.
defPipelineGroup ::
  NameSymbol.T -> [CircularList.T Step.Named] -> CircularList.T Step.Named
defPipelineGroup sym = CircularList.groupOf sym

-- | Tell the environment to stop at a particular step when running
-- the environment.
stopAt :: NameSymbol.T -> EnvS ()
stopAt = put @"stoppingStep" . Just

-- | @stopAtNothing@ tells the environment to run the compiler fully.
stopAtNothing :: EnvS ()
stopAtNothing = put @"stoppingStep" Nothing

--------------------------------------------------------------------------------
-- Important evaluation functions
--------------------------------------------------------------------------------

empty :: Pipeline.CIn -> T
empty information =
  T {pipeline = CircularList.empty, stoppingStep = Nothing, information}

fullyEmpty :: IO T
fullyEmpty =
  Context.empty "Juvix"
    >>| Pipeline.WorkingEnv []
    >>| Pipeline.emptyInput
    >>| empty

-- | @run@ serves as the running point, the @EnvS b@ argument is the
-- monadic function used to build up the environment, and the
-- @Pipeline.CIn@ is the given pre-built environment
run :: EnvS b -> Pipeline.CIn -> IO Pipeline.CIn
run env = eval . extract env . empty

-- | @extract@ extracts the @T@ value from a staring environment, @T@,
-- and a series of computation @EnvS@ that represents the registration
-- of passes making up a new environment @T@.
extract :: EnvS a -> T -> T
extract (EnvS st) = execState st

-- | @eval@ is responsible for taking the environment, running it to
-- the desired point and giving back what data is left.
eval :: T -> IO Pipeline.CIn
eval T {information = input, pipeline, stoppingStep} = do
  case nextStep of
    Nothing -> pure input
    Just (CircularList.NonCircSchema Step.Named {name, step})
      | atStoppingStep name stoppingStep || atStoppingGroup stepGroups stoppingStep ->
        pure input
      | otherwise -> do
        res <- Step.call step (Pipeline.setNameCIn name input)
        --
        let information = reconstructInput input res
        --
        case shouldContinue res of
          True -> eval T {information, pipeline = remainder, stoppingStep}
          False -> pure information
    -- We need to think how we want to deal with circular Schemas in general
    Just CircularList.CircSchema {} ->
      notImplemented
  where
    stepGroups = CircularList.namesToFirstTerm pipeline
    nextStep = CircularList.firstNested pipeline
    remainder = CircularList.removeFirstNested pipeline

------------------------------------------------------------
-- Helping functions for eval
------------------------------------------------------------

-- | @atStoppingStep@ determines if the current step of the piepline
-- is the step that we should stop evaluating at. If no stopping step
-- is specified this will always return False.
atStoppingStep :: Eq a => a -> Maybe a -> Bool
atStoppingStep currentStep = maybe False (== currentStep)

-- | @atStoppingStep@ determines if the current step of the piepline
-- is apart of the group that we should stop evaluating at. If no
-- stopping step is specified this will always return False.
atStoppingGroup :: (Eq a, Hashable a) => Set.HashSet a -> Maybe a -> Bool
atStoppingGroup set = maybe False (`Set.member` set)

-- | @shouldContinue@ determines if we should continue on the next
-- step of the pipeline, or if there is an error and we should going
-- forward
shouldContinue :: Pipeline.COut Pipeline.WorkingEnv -> Bool
shouldContinue Pipeline.Success {} = True
shouldContinue Pipeline.Failure {} = False -- if we get back data it might be True

-- | @reconstructInput@ reconstructs a computational input from an
-- output and an existing input. In the case the step
-- succeeds the computational input's @WorkingEnv@ goes unused.
reconstructInput ::
  Pipeline.CIn -> Pipeline.COut Pipeline.WorkingEnv -> Pipeline.CIn
reconstructInput cin res =
  cin
    |> set (surroundingData . currentStepName) Nothing
    |> set (surroundingData . metaInfo) (res ^. meta)
    |> set languageData newLanguageData
  where
    newLanguageData =
      case res of
        Pipeline.Success {_result = work} -> work
        Pipeline.Failure {_partialResult} ->
          fromMaybe (cin ^. languageData) _partialResult
