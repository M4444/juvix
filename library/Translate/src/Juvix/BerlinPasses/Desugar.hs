module Juvix.BerlinPasses.Desugar
  ( condPass,
    modulePass,
    headerPass,
    moduleLetPass,
    ifPass,
    letPass,
    multipleDefunPass,
    combineSigPass,
    removePunnedRecordsPass,
    handlerPass,
  )
where

import Control.Lens (over, (^.))
import qualified Juvix.BerlinPasses.Environment as Env
import qualified Juvix.BerlinPipeline.Automation as Automation
import Juvix.BerlinPipeline.Lens
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.Step as Step
import qualified Juvix.Context as Context
import qualified Juvix.Desugar.Passes as Desugar.Passes
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp
import Juvix.Sexp.Structure.Lens
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure

--------------------------------------------------------------------------------
-- Simlplified Passes
--------------------------------------------------------------------------------

condPass :: Step.Named
condPass =
  (Trace.withScope "Desugar.cond-runner" [] . Automation.simplify condTrans)
    |> Automation.runSimplifiedPass
    |> Step.T
    |> Step.namePass "Desugar.cond-to-if"

condTrans :: Automation.SimplifiedPassArgument -> Env.MinimalMIO Automation.Job
condTrans simplify = do
  Trace.withScope "Desugar.condTrans" [show (simplify ^. current)] $ do
    condTransform (simplify ^. current)
      >>| (`Automation.ProcessNoEnv` [])
      >>| Automation.ProcessJob

-- | @condTransform@ - CondTransform turns the cond form of the fronted
-- language into a series of ifs
-- - BNF input form:
--   + (:cond (pred-1 result-1) … (pred-n result-n))
-- - BNF output form:
--   + (if pred-1 result-1 (if pred-2 result-2 (… (if pred-n result-n))))
-- condTransform :: Sexp.T -> Sexp.T
condTransform :: (MonadIO m, Meta.HasMeta m) => Sexp.T -> m Sexp.T
condTransform xs =
  Trace.withScope "Desugar.condTransform" [show xs] $ do
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
    condToIf _ _ = Env.throw $ Env.MalformedData "cond is in an invalid format"
    --
    generation predAns acc =
      Structure.If (predAns ^. predicate) (predAns ^. answer) acc
        |> Structure.fromIf

modulePass :: Step.Named
modulePass = desugarPass Desugar.Passes.moduleTransform "module"

moduleLetPass :: Step.Named
moduleLetPass = desugarPass Desugar.Passes.moduleLetTransform "moduleLet"

ifPass :: Step.Named
ifPass = desugarPass Desugar.Passes.ifTransform "if"

letPass :: Step.Named
letPass = desugarPass Desugar.Passes.multipleTransLet "let"

removePunnedRecordsPass :: Step.Named
removePunnedRecordsPass =
  desugarPass Desugar.Passes.removePunnedRecords "removePunnedRecords"

handlerPass :: Step.Named
handlerPass = desugarPass Desugar.Passes.handlerTransform "handlerTransform"

--------------------------------------------------------------------------------
-- Header pass
--------------------------------------------------------------------------------

headerPass :: Step.Named
headerPass =
  (Trace.withScope "Desugar.header-runner" [] . Automation.simplify headerTrans)
    |> (\f -> Automation.runSimplifiedPass f . injectCurrentPackageContext)
    |> Step.T
    |> Step.namePass "Desugar.header"

headerTrans :: Automation.SimplifiedPassArgument -> Env.MinimalMIO Automation.Job
headerTrans simplify =
  Trace.withScope "Desugar.headerTrans" [show (simplify ^. current)] $
    headerTransform (simplify ^. current)
      >>| (\(h, sexps) -> Automation.ProcessNoEnv h (fmap f sexps))
      >>| Automation.ProcessJob
  where
    f s = (Automation.Current, s)

-- TODO: Make this recursive?
headerTransform :: (Meta.HasMeta m) => Sexp.T -> m (Sexp.T, [Sexp.T])
headerTransform sexp = case Structure.toHeader sexp of
  Nothing -> (sexp, []) |> pure
  Just (Structure.Header name xs) ->
    case Sexp.toList @Maybe xs of
      Just sexps ->
        (Sexp.serialize (Structure.InPackage (Context.addTopName name)), sexps)
          |> pure
      Nothing -> Env.throw $ Env.MalformedData "header is in an invalid format"

--------------------------------------------------------------------------------
-- Multiple Defun
--------------------------------------------------------------------------------

multipleDefunPass :: Step.Named
multipleDefunPass =
  (pure . transMultiSexpNoContext Desugar.Passes.multipleTransDefun)
    |> Step.T
    |> Step.namePass "Context.multipleDefun"

--------------------------------------------------------------------------------
-- Combine Sig
--------------------------------------------------------------------------------

combineSigPass :: Step.Named
combineSigPass =
  pure . transMultiSexpNoContext Desugar.Passes.combineSig
    |> Step.T
    |> Step.namePass "Context.combineSig"

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

desugarPass ::
  (Sexp.T -> Sexp.T) ->
  NameSymbol.T ->
  Step.Named
desugarPass trans name =
  (Trace.withScope scopeName [] . Automation.simplify f)
    |> Automation.runSimplifiedPass
    |> Step.T
    |> Step.namePass passName
  where
    f = desugarTrans trans name
    passName = "Desugar" <> name
    scopeName = passName <> "runner"

desugarTrans ::
  (Sexp.T -> Sexp.T) ->
  NameSymbol.T ->
  Automation.SimplifiedPassArgument ->
  Env.MinimalMIO Automation.Job
desugarTrans trans name simplify = do
  Trace.withScope ("Desugar" <> name) [show (simplify ^. current)] $ do
    (pure . trans) (simplify ^. current)
      >>| (`Automation.ProcessNoEnv` [])
      >>| Automation.ProcessJob

injectCurrentPackageContext :: Pipeline.CIn -> Pipeline.CIn
injectCurrentPackageContext cin =
  over (languageData . currentExp) f cin
  where
    f =
      (cin ^. languageData . context)
        |> Context.currentName
        |> Context.addTopName
        |> Structure.InPackage
        |> Sexp.serialize
        |> Pipeline.Sexp
        |> (:)

transMultiSexpNoContext ::
  ([Sexp.T] -> [Sexp.T]) ->
  Pipeline.CIn ->
  Pipeline.COut Pipeline.WorkingEnv
transMultiSexpNoContext trans cin =
  Pipeline.Success meta newWenv
  where
    wenv = cin ^. languageData
    sexps = (wenv ^. currentExp) >>= f
    meta = cin ^. surroundingData . metaInfo

    newSexps = trans sexps >>| Pipeline.Sexp

    newWenv =
      Pipeline.WorkingEnv
        { _currentExp = newSexps,
          _context = wenv ^. context
        }

    -- This pass runs before Context is initialized
    f (Pipeline.InContext _) = []
    f (Pipeline.Sexp sexp) = [sexp]
