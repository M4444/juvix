module Juvix.Desugar.Env where

import qualified Juvix.BerlinPipeline.Automation as Automation
import qualified Juvix.BerlinPipeline.Feedback as Feedback
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.Env as Pipeline.Env
import Juvix.Library hiding (trace)
import Prelude (error)
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure
import qualified Juvix.BerlinPipeline.Step as Step
import qualified Juvix.BerlinPipeline.CircularList as CircularList
import Control.Lens hiding ((|>))
import Juvix.Sexp.Structure.Lens
import qualified Juvix.Context as Context
--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

data Env = Env
  { trace :: Trace.T,
    feedback :: Feedback.T
  }
  deriving (Generic, Show)

type MinimalAliasIO =
  ExceptT Sexp.T (StateT Env IO)

newtype MinimalMIO a = MinIO {_runIO :: MinimalAliasIO a}
  deriving (Functor, Applicative, Monad, MonadIO)
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

data Error = MalformedData Text
           deriving (Generic, Show, Eq)

instance Sexp.DefaultOptions Error
instance Sexp.Serialize Error

runEnv :: MinimalMIO a -> Meta.T -> IO (Either Sexp.T a, Env)
runEnv (MinIO a) (Meta.T {trace, feedback}) =
  runStateT (runExceptT a) Env {trace, feedback}

-- TODO ∷ update to use feedback before throwing. (Sadly not finished)
throw :: (HasThrow "error" Sexp.T m) => Error -> m a2
throw = Juvix.Library.throw @"error" . Sexp.serialize

-- Do not use externally
runEnvEmpty :: MinimalMIO a -> IO (Either Sexp.T a, Env)
runEnvEmpty (MinIO a) =
  runStateT (runExceptT a) Env {trace = Trace.empty, feedback = Feedback.empty}

instance Pipeline.HasExtract MinimalMIO where
  -- it's fine to run empty as the meta information gets injected in
  extract a = do
    (either, env) <- runEnvEmpty a
    let meta = Meta.T {trace = trace env, feedback = feedback env}
    case either of
      -- goes unused, see feedback for the data
      Left _sexp ->
        Pipeline.Failure {meta, partialResult = Nothing} |> pure
      Right data' ->
        Pipeline.Success {meta, result = data'} |> pure

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

Right sexp = Sexp.parse "(defun foo (x) (:cond (pred-1 (:cond (pred-1 result-1) (pred-n result-n))) (pred-n result-n)))"

Right secondSexp = Sexp.parse "(defun foo (x) (+ x 1))"

startingEnv =
  (Context.empty "JU-USER" ∷ IO (Context.T Sexp.T Sexp.T Sexp.T))
  >>| Pipeline.WorkingEnv [Pipeline.Sexp sexp, Pipeline.Sexp secondSexp]


example = do
  Pipeline.CIn languageData surroudning <- startingEnv >>= Pipeline.Env.run eval . Pipeline.emptyInput
  -- print languageData
  Pipeline.metaInfo surroudning |> Meta.trace |> Trace.info

eval :: Pipeline.Env.EnvS ()
eval = do
  Pipeline.Env.registerStep (CircularList.init condPass)


condPass :: Step.Named
condPass =
  Automation.runSimplifiedPass (\arg -> Trace.withScope "Desugar.cond-runner" [] (Automation.simplify condTrans arg))
  |> Step.T
  |> Step.namePass "Desugar.cond-to-if"

condTrans :: Automation.SimplifiedPassArgument -> MinimalMIO Automation.Job
condTrans simplify = do
  Trace.withScope "Desguar.condTrans" [show (simplify ^. Automation.current)] $ do
    Trace.traceAllEff
    condTransform (simplify ^. Automation.current)
      >>| (\transformed -> Automation.ProcessNoEnv transformed [])
      >>| Automation.ProcessJob


 -- | @condTransform@ - CondTransform turns the cond form of the fronted
 -- language into a series of ifs
 -- - BNF input form:
 --   + (:cond (pred-1 result-1) … (pred-n result-n))
 -- - BNF output form:
 --   + (if pred-1 result-1 (if pred-2 result-2 (… (if pred-n result-n))))
-- condTransform :: Sexp.T -> Sexp.T
condTransform :: (MonadIO m, HasState "trace" Trace.T m, HasThrow "error" Sexp.T m) => Sexp.T -> m Sexp.T
condTransform xs =
  Trace.withScope "Desguar.condTransform" [show xs] $ do
    Sexp.traversePredStar xs (== Structure.nameCond) condToIf
  where
    condToIf sexp@(Sexp.Atom atom Sexp.:> _) recur
      | Just cond <- Structure.toCond sexp,
        Just last <- lastMay (cond ^. entailments) =
        let acc =
              Structure.IfNoElse (last ^. predicate) (last ^. answer)
                |> Structure.fromIfNoElse
         in foldr generation acc (initSafe (cond ^. entailments))
              |> Sexp.addMetaToCar atom
              |> recur
    condToIf _ _ = Juvix.Desugar.Env.throw $ MalformedData "malformed cond"
    --
    generation predAns acc =
      Structure.If (predAns ^. predicate) (predAns ^. answer) acc
        |> Structure.fromIf

