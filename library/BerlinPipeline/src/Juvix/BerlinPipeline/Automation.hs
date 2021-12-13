module Juvix.BerlinPipeline.Automation where

import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Context as Context
import Juvix.Library
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
