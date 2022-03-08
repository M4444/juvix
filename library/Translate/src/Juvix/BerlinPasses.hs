module Juvix.BerlinPasses
  ( eval,
    desugarPasses,
    contextPasses,
  )
where

import Control.Lens hiding ((|>))
import qualified Juvix.BerlinPasses.Contextify as Contextify
import qualified Juvix.BerlinPasses.Desugar as Desugar
import qualified Juvix.BerlinPipeline.Automation as Automation
import qualified Juvix.BerlinPipeline.CircularList as CircularList
import qualified Juvix.BerlinPipeline.Env as Pipeline.Env
import Juvix.BerlinPipeline.Lens
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.Step as Step
import qualified Juvix.Context as Context
import qualified Juvix.Context as Context.Pass
import qualified Juvix.Contextify.Passes as Context.Pass
import Juvix.Library hiding (trace)
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Transition as Structure

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

eval :: Pipeline.Env.EnvS ()
eval = do
  Pipeline.Env.registerAfterEachStep inPackageTrans
  Pipeline.Env.registerStep desugarPasses
  Pipeline.Env.registerStep contextPasses
  Pipeline.Env.registerStep qualificationPasses

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

qualificationPasses :: CircularList.T Step.Named
qualificationPasses =
  [ Context.Pass.qualifyPass
  ]
    >>| CircularList.init
    |> Pipeline.Env.defPipelineGroup "QualifyPasses"

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
