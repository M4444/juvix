module Juvix.BerlinPipeline.Automation where

import qualified Control.Lens as Lens hiding ((|>))
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
      { processCurrent :: Pipeline.EnvOrSexp,
        processNewForms :: [(Stage, Pipeline.EnvOrSexp)]
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

data SimplePassArg
  = SimplePassArg
      { simplePassArgContext :: Context.T Sexp.T Sexp.T Sexp.T,
        simplePassArgCurrent :: Sexp.T
      } deriving (Show, Eq, Generic)

Lens.makeLensesWith Lens.camelCaseFields ''SimplePassArg

data PassArg
  = PassArg
      { passContext :: Context.T Sexp.T Sexp.T Sexp.T,
        passCurrent :: Pipeline.EnvOrSexp
      } deriving (Show, Eq, Generic)

transformOutputType :: Job -> Pipeline.WorkingEnv
transformOutputType = notImplemented

-- we share the one m monad here for the function
-- because we will extract it after we run the m effect
applySimplifiedPass ::
  --   (HasTrace m a, HasFeedback m a) =>
  (PassArg -> m Job) ->
  -- These form Pipeline.Step.t for some m over the
  -- output modulo the ComputationResult.t over it
  Pipeline.CIn ->
  m Pipeline.WorkingEnv
applySimplifiedPass = notImplemented

runSimplifiedPass ::
  -- (HasExtract a m) =>
  (PassArg -> m Job) ->
  Pipeline.CIn ->
  m (Pipeline.COut Pipeline.WorkingEnv)
runSimplifiedPass f = do
  extract . applySimplifiedPass f
  where
    extract = notImplemented

-- | @simplify allows a pass to ignore the fact that expression coming in may
-- be added to the [Context.T] already, and we can act as if it were just a
-- normal [Sexp.T] being passed in.
simplify
  :: Monad m
  => (SimplePassArg -> m Job)
  -> (PassArg -> m Job)
simplify f PassArg { passContext, passCurrent} =
  undefined
  -- case passCurrent of
  -- Pipeline.InContext n -> case Context.lookup n passContext of
  --   Just s -> do
  --     result <- f (SimplePassArg passContext s)
  --     case result of
  --       PJob (ProcessJobNoEnv { neCurrent, neNewForms }) -> do
  --         let c = Context.add n neCurrent passContext
  --             forms = second Pipeline.Sexp neNewForms
  --         pure $ PJob (UpdateJob c (ProcessJob (Pipeline.InContext n forms)))
  --       UJob (UpdateJob context process) -> do
  --         let c = Context.add n neCurrent context
  --         pure $ PJob (UpdateJob c process)
  --   Nothing -> panic "FIX ME"
  -- Pipeline.Sexp s -> f (SimplePassArg passContext s)

-- deconstructPass :: Step.Named
-- deconstructPass =
--   Step.namePass
--     (runSimplifiedPass deconstructType)
--     "Desugar.deconstruct-type"

-- deconstructType PassArg {passCurrent} =
--   Trace.with "Desugar.deconstruct-type" [show passCurrent] $
--     Sexp.foldSearchPredWithExtra
--       (simplify f) (== Structure.typeName) current
--     |> \Sexp.Extra{data, extra} ->
--        ProcessJob {current = data, newForms = extra}
--   where
--     f car cdr =
--       Trace.with "Desugar.deconstruct-type-pass" [show car, show cdr] $
--         case Structure.toType (car Sexp.:> cdr) of
--           Just typ ->
--             let extraData =
--               (typ ^. body)
--                >>= (\case Structure.Sum {name, arguments} ->
--                             Structure.Constructor name (typ ^. name) arguments
--                             |> pure
--                           _ -> []
--                    )
--                >>| \x -> (Automation.Current, Structure.fromConstructor x)

--             let currentForm =
--                Structure.DeclareType (typ ^. name) (typ ^. arguments)
--                |> Structure.fromDeclareType

--             Structure.Extra {data = currentForm, extra = extraData}
--           Nothing ->
--            -- we get back a Failure on the ComputationResult
--            throw @"failure" (ComputationResult.InvalidForm (car Sexp.:> cdr))
