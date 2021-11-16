module Juvix.BerlinPipeline.Env where

import qualified Juvix.BerlinPipeline.CircularList as CircularList
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.RecursiveList as RecursiveList
import qualified Juvix.BerlinPipeline.Step as Step
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T = T
  { information :: Pipeline.ComputationalInput,
    registeredPipeline :: CircularList.T (Step.Named),
    stoppingStep :: Maybe NameSymbol.T
  }
  deriving (Generic)

newtype EnvS b = EnvS (State T b)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "information" (Pipeline.ComputationalInput),
      HasSink "information" (Pipeline.ComputationalInput),
      HasSource "information" (Pipeline.ComputationalInput)
    )
    via StateField "information" (State T)
  deriving
    ( HasState "registeredPipeline" (CircularList.T (Step.Named)),
      HasSink "registeredPipeline" (CircularList.T (Step.Named)),
      HasSource "registeredPipeline" (CircularList.T (Step.Named))
    )
    via StateField "registeredPipeline" (State T)
  deriving
    ( HasState "stoppingStep" (Maybe NameSymbol.T),
      HasSink "stoppingStep" (Maybe NameSymbol.T),
      HasSource "stoppingStep" (Maybe NameSymbol.T)
    )
    via StateField "stoppingStep" (State T)

data StopADT = Stop

-- | Registers the pipeline function to the environment
registerStep :: CircularList.T Step.Named -> EnvS ()
registerStep l = do
  modify @"registeredPipeline" $ \i -> i <> l

-- | @defPipelineGroup@ creates a named group of pipeline steps or nested grouping of pipeline steps.
defPipelineGroup :: NameSymbol.T -> [CircularList.T Step.Named] -> CircularList.T Step.Named
defPipelineGroup sym ls = foldl' (flip (<>)) (CircularList.init sym) ls

-- | Tells the environment to stop at a particular step when running the environment.
stopAt :: NameSymbol.T -> EnvS ()
stopAt sym = put @"stoppingStep" (Just sym)

stopAtNothing :: EnvS ()
stopAtNothing = put @"stoppingStep" Nothing

eval :: Monad m => T -> m Pipeline.ComputationalInput
eval (T information registeredPipeline stoppingStep) = notImplemented

run :: EnvS b -> T -> Pipeline.ComputationalInput
run = notImplemented

extract :: EnvS b -> T
extract = notImplemented
