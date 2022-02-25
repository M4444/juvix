module Juvix.Desugar.Env where

import Control.Lens hiding ((|>))
import qualified Juvix.BerlinPasses.Contextify as Contextify
import qualified Juvix.BerlinPasses.Desugar as Desugar
import qualified Juvix.BerlinPasses.Environment as Env
import qualified Juvix.BerlinPipeline.Automation as Automation
import qualified Juvix.BerlinPipeline.CircularList as CircularList
import qualified Juvix.BerlinPipeline.Env as Pipeline.Env
import qualified Juvix.BerlinPipeline.Feedback as Feedback
import Juvix.BerlinPipeline.Lens
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.Step as Step
import qualified Juvix.Context as Context
import Juvix.Library hiding (trace)
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Transition as Structure

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

Right sexp =
  Sexp.parse
    "(:defun foo (x) (:cond (pred-1 (:cond (pred-1 result-1) (pred-n result-n))) (pred-n result-n)))"

Right secondSexp = Sexp.parse "(:defun foo (x) (+ x 1))"

startingEnv :: IO Pipeline.WorkingEnv
startingEnv =
  Context.empty "JU-USER"
    >>| Pipeline.WorkingEnv [Pipeline.Sexp sexp, Pipeline.Sexp secondSexp]

exampleMeta :: IO Meta.T
exampleMeta = do
  Pipeline.CIn _ surroudning <-
    startingEnv
      >>= Pipeline.Env.run eval
        . Pipeline.modifyTraceCIn
          (`Trace.enable` ["Desugar.cond-runner", "Desugar.condTransform"])
        -- . Pipeline.modifyTraceCIn Trace.traceAll
        . Pipeline.emptyInput
  surroudning ^. Pipeline.metaInfo |> pure

exampleCtx :: IO Pipeline.WorkingEnv
exampleCtx = do
  Pipeline.CIn languageData _ <-
    startingEnv
      >>= Pipeline.Env.run eval
        . Pipeline.modifyTraceCIn
          (`Trace.enable` ["Desugar.cond-runner", "Desugar.condTransform"])
        -- . Pipeline.modifyTraceCIn Trace.traceAll
        . Pipeline.emptyInput
  languageData |> pure

example :: IO ()
example = do
  exampleMeta >>= Meta.info

exampleIndexing :: IO (Maybe Env.Error)
exampleIndexing = do
  myValue <- exampleMeta
  Feedback.contentsAt 0 (myValue ^. Meta.feedback)
    |> Sexp.deserialize @Env.Error
    |> pure

eval :: Pipeline.Env.EnvS ()
eval = do
  Pipeline.Env.registerAfterEachStep inPackageTrans
  Pipeline.Env.registerStep desugarPasses
  Pipeline.Env.registerStep contextPasses

desugarPasses :: CircularList.T Step.Named
desugarPasses =
  [ Desugar.headerPass,
    Desugar.moduleLetPass,
    Desugar.modulePass,
    Desugar.condPass,
    Desugar.ifPass,
    Desugar.letPass,
    Desugar.multipleDefunPass,
    Desugar.combineSigPass,
    Desugar.removePunnedRecordsPass,
    Desugar.handlerPass
  ]
    >>| CircularList.init
    |> Pipeline.Env.defPipelineGroup "DesugarPasses"

contextPasses :: CircularList.T Step.Named
contextPasses =
  [ Contextify.initContextPass,
    Contextify.resolveModulePass,
    Contextify.infixConversionPass,
    Contextify.recordDeclarationPass,
    Contextify.unknownSymbolLookupPass
  ]
    >>| CircularList.init
    |> Pipeline.Env.defPipelineGroup "ContextPasses"

--------------------------------------------------------------------------------
-- InPackage
--------------------------------------------------------------------------------

-- | @inPackageTrans@ - If the current Sexp is an `:in-package` form then this function
-- updates the current Context to the corresponding namespace.
inPackageTrans ::
  MonadIO m => Automation.PassArgument -> m Automation.PassArgument
inPackageTrans arg = case arg ^. current of
  (Pipeline.InContext _) -> noOp
  (Pipeline.Sexp sexp) -> Sexp.deserialize sexp |> maybe noOp f
  where
    f (Structure.InPackage name) = do
      newCtx <-
        liftIO $
          Context.switchNameSpace name ctx
            >>| either (const ctx) identity
      set context newCtx arg |> pure
    noOp = arg |> pure
    ctx = arg ^. context
