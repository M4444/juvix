module Juvix.BerlinPipeline.Automation where

import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

data Job
  = PJob ProcessJobNoEnv
  | UJob UpdateJob
  deriving (Show, Eq, Generic)

data UpdateJob
  = UpdateJob
      { newContext :: Context.T Sexp.T Sexp.T Sexp.T,
        process :: ProcessJob
      }
  deriving (Show, Eq, Generic)

data Stage
  = Current
  | FromTopToCurrent
  | Eval
  deriving (Show, Eq, Generic)

data ProcessJob
  = ProcessJob
      { current :: Pipeline.EnvOrSexp,
        newForms :: [(Stage, Pipeline.EnvOrSexp)]
      }
  deriving (Show, Eq, Generic)

data ProcessJobNoEnv
  = ProcessJobNoEnv
      { neCurrent :: Sexp.T,
        neNewForms :: [(Stage, Sexp.T)]
      }
  deriving (Show, Eq, Generic)

class Functor m => Comonad m where
  run :: m a -> a

type HasTrace m = HasState "trace" Meta.Trace

type HasFeedback m = HasState "feedback" Meta.Feedback

class HasExtract a m | a -> m where
  extract :: a x -> m (Pipeline.COut x)

data PassArgument
  = PassArg
      { passContext :: Context.T Sexp.T Sexp.T Sexp.T,
        passCurrent :: Sexp.T
      }

transformOutputType :: Job -> Pipeline.WorkingEnv
transformOutputType = notImplemented

-- we share the one m monad here for the function
-- because we will extract it after we run the m effect
applySimplifiedPass ::
  --   (HasTrace m a, HasFeedback m a) =>
  (PassArgument -> m Job) ->
  -- These form Pipeline.Step.t for some m over the
  -- output modulo the ComputationResult.t over it
  Pipeline.CIn ->
  m Pipeline.WorkingEnv
applySimplifiedPass = notImplemented

runSimplifiedPass ::
  --   (HasExtract _a m) =>
  (PassArgument -> m Job) ->
  Pipeline.CIn ->
  m (Pipeline.COut Pipeline.WorkingEnv)
runSimplifiedPass f = do
  extract . applySimplifiedPass f
  where
    extract = notImplemented
