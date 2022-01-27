{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.BerlinPipeline.Env where

import qualified Data.HashSet as Set
import qualified Juvix.BerlinPipeline.CircularList as CircularList
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.Step as Step
import qualified Juvix.Context as Context
import Juvix.Library hiding (empty)
import qualified Juvix.Library.NameSymbol as NameSymbol

--------------------------------------------------------------------------------
-- Type Specification
--------------------------------------------------------------------------------

data T = T
  { information :: Pipeline.CIn,
    -- TODO :: rename to working environment, as this gets consumed
    -- over time
    pipeline :: CircularList.T (Step.Named),
    -- TODO :: add a FullPipeline, that does not get consumed.
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

run :: EnvS b -> Pipeline.CIn -> IO Pipeline.CIn
run (EnvS st) = eval . execState st . empty

extract :: EnvS a -> T -> T
extract (EnvS st) = execState st

-- | @eval@ is responsible for taking the environment, running it to
-- the desired point and giving back what data is left.
eval :: T -> IO Pipeline.CIn
eval T {information = input@Pipeline.CIn {languageData}, pipeline, stoppingStep} = do
  case nextStep of
    Nothing -> pure input
    Just (CircularList.NonCircSchema Step.Named {name, step})
      | atStoppingStep name stoppingStep || atStoppingGroup stepGroups stoppingStep ->
        pure input
      | otherwise -> do
        res <- Step.call step (Pipeline.setNameCIn name input)
        --
        let information = reconstructInput languageData res
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
-- output and an existing work environment. In the case the step
-- succeeds the computational input goes unused.
reconstructInput ::
  Pipeline.WorkingEnv -> Pipeline.COut Pipeline.WorkingEnv -> Pipeline.CIn
reconstructInput workEnv res =
  Pipeline.CIn {surroundingData = newEnv (Pipeline.getMeta res), languageData}
  where
    newEnv = Pipeline.SurroundingEnv Nothing
    languageData =
      case res of
        Pipeline.Success {result = work} -> work
        Pipeline.Failure {partialResult} -> fromMaybe workEnv partialResult
