module Juvix.Environment
  ( Error (..),
    Env (..),
    MinimalAliasIO,
    MinimalMIO (..),
    runEnv,
    Juvix.Environment.throw,
    HasClosure,
  )
where

import qualified Juvix.BerlinPipeline.Feedback as Feedback
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Closure as Closure
import Juvix.Library hiding (trace)
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp

newtype Error = MalformedData Text
  deriving (Generic, Show, Eq)

instance Sexp.DefaultOptions Error

instance Sexp.Serialize Error

runEnv :: MinimalMIO a -> Meta.T -> IO (Either Sexp.T a, Env)
runEnv (MinIO a) Meta.T {_trace, _feedback} =
  runStateT
    (runExceptT a)
    Env
      { trace = _trace,
        feedback = _feedback,
        closure = Closure.empty
      }

throw :: (Feedback.Eff m, HasThrow "error" Sexp.T m) => Error -> m a2
throw err = do
  Feedback.error (Sexp.serialize err)
  Juvix.Library.throw @"error" (Sexp.serialize err)

--------------------------------------------------------------------------------
-- Runner environment
--------------------------------------------------------------------------------

data Env = Env
  { trace :: Trace.T,
    feedback :: Feedback.T,
    closure :: Closure.T
  }
  deriving (Generic, Show)

type MinimalAliasIO =
  ExceptT Sexp.T (StateT Env IO)

newtype MinimalMIO a = MinIO {_runIO :: MinimalAliasIO a}
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasReader "closure" Closure.T,
      HasSource "closure" Closure.T
    )
    via ReaderField "closure" MinimalAliasIO
  deriving
    ( HasState "feedback" Feedback.T,
      HasSource "feedback" Feedback.T,
      HasSink "feedback" Feedback.T
    )
    via StateField "feedback" MinimalAliasIO
  deriving
    ( HasState "trace" Trace.T,
      HasSource "trace" Trace.T,
      HasSink "trace" Trace.T
    )
    via StateField "trace" MinimalAliasIO
  deriving
    (HasThrow "error" Sexp.T)
    via MonadError MinimalAliasIO

type HasClosure m = HasReader "closure" Closure.T m

runEnvEmpty :: MinimalMIO a -> IO (Either Sexp.T a, Env)
runEnvEmpty (MinIO a) =
  runStateT
    (runExceptT a)
    Env
      { trace = Trace.empty,
        feedback = Feedback.empty,
        closure = Closure.empty
      }

instance Pipeline.HasExtract MinimalMIO where
  -- it's fine to run empty as the meta information gets injected in
  extract a = do
    (either, env) <- runEnvEmpty a
    let meta = Meta.T {_trace = trace env, _feedback = feedback env}
    case either of
      -- goes unused, see feedback for the data
      Left _sexp ->
        Pipeline.Failure {_meta = meta, _partialResult = Nothing} |> pure
      Right data' ->
        Pipeline.Success {_meta = meta, _result = data'} |> pure
