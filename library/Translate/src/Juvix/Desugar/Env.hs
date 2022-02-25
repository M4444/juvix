module Juvix.Desugar.Env where

import Control.Lens hiding ((|>))
import qualified Data.List.NonEmpty as NonEmpty
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
import qualified Juvix.Context.Traversal as Context
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.Passes as Passes
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as ResolveOpen
import Juvix.Library hiding (trace)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp
import Juvix.Sexp.Structure.Lens
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure
import Prelude (error)

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
  Pipeline.Env.registerStep (CircularList.init initContextPass)
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
  [ resolveModulePass,
    infixConversionPass,
    recordDeclarationPass,
    unknownSymbolLookupPass
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

--------------------------------------------------------------------------------
-- Resolve Modules
--------------------------------------------------------------------------------

resolveModulePass :: Step.Named
resolveModulePass = mkPass name trans
  where
    name = "resolveModule"
    trans = mkTrans name resolveModuleTransform

resolveModuleTransform ::
  ( HasThrow "error" Sexp.T m,
    Feedback.Eff m,
    Env.HasClosure m,
    MonadIO m
  ) =>
  Context.T ->
  Sexp.T ->
  m Sexp.T
resolveModuleTransform ctx =
  Passes.openResolution ctx |> traverseOnDeserialized

--------------------------------------------------------------------------------
-- Infix Form Transformation
--------------------------------------------------------------------------------

infixConversionPass :: Step.Named
infixConversionPass = mkPass name trans
  where
    name = "infixConversion"
    trans = mkTrans name infixConversionTransform

infixConversionTransform ::
  ( HasThrow "error" Sexp.T m,
    Feedback.Eff m,
    Env.HasClosure m,
    MonadIO m
  ) =>
  Context.T ->
  Sexp.T ->
  m Sexp.T
infixConversionTransform ctx =
  Passes.infixConversion ctx |> traverseOnDeserialized

--------------------------------------------------------------------------------
-- Record Lookup from Unknown Symbols
--------------------------------------------------------------------------------

unknownSymbolLookupPass :: Step.Named
unknownSymbolLookupPass = mkPass name trans
  where
    name = "unknownSymbolLookup"
    trans = mkTrans name unknownSymbolLookupTransform

unknownSymbolLookupTransform ::
  ( HasThrow "error" Sexp.T m,
    Feedback.Eff m,
    Env.HasClosure m,
    MonadIO m
  ) =>
  Context.T ->
  Sexp.T ->
  m Sexp.T
unknownSymbolLookupTransform ctx =
  Passes.primiveOrSymbol ctx |> traverseOnDeserialized

--------------------------------------------------------------------------------
-- Helpers for Constructing Passes
--------------------------------------------------------------------------------

mkTrans ::
  NameSymbol.T ->
  -- | The name of the transform to be traced
  ( Context.T ->
    Sexp.T ->
    Env.MinimalMIO Sexp.T
  ) ->
  Automation.SimplifiedPassArgument ->
  Env.MinimalMIO Automation.Job
mkTrans name trans simplify =
  Trace.withScope scopeName [show (simplify ^. current)] $
    trans (simplify ^. context) (simplify ^. current)
      >>| (`Automation.ProcessNoEnv` [])
      >>| Automation.ProcessJob
  where
    scopeName = "Context" <> name <> "trans"

mkPass ::
  NameSymbol.T ->
  -- | The name of the transform to be traced
  ( Automation.SimplifiedPassArgument ->
    Env.MinimalMIO Automation.Job
  ) ->
  Step.Named
mkPass name trans =
  ( Trace.withScope
      runnerName
      []
      . simplifiedTrans
  )
    |> Automation.runSimplifiedPass
    |> Step.T
    |> Step.namePass passName
  where
    simplifiedTrans = Automation.simplify trans
    runnerName = passName <> "runner"
    passName = "Context" <> name

--------------------------------------------------------------------------------
-- Record Recognition Transformation
--------------------------------------------------------------------------------

recordDeclarationPass :: Step.Named
recordDeclarationPass =
  ( Trace.withScope
      "Context.recordDeclaration-runner"
      []
      . recordDeclaration
  )
    |> Automation.runSimplifiedPass
    |> Step.T
    |> Step.namePass "Context.recordDeclaration"

recordDeclaration ::
  Pipeline.PassArgument ->
  Env.MinimalMIO Pipeline.Job
recordDeclaration pa =
  case pa ^. current of
    Pipeline.InContext name ->
      case ctx Context.!? name of
        Just from -> do
          let qualifiedName = from ^. Context.qualifedName
          newRecord <-
            from ^. Context.term
              |> Passes.figureRecord
              >>| Context.formPutBack
          updateJob qualifiedName newRecord |> pure
        Nothing ->
          Juvix.Library.throw @"error" $
            Sexp.string
              "Could not find definition when \
              \ it was promised to be in the environment"
    _ -> noOpJob |> pure
  where
    ctx = pa ^. context
    noOpJob = Automation.noOpJob ctx (pa ^. current)
    updateJob name info =
      Pipeline.UpdateJob
        { newContext = Context.addGlobal name info ctx,
          process =
            Pipeline.Process
              { _current = Pipeline.InContext name,
                _newForms = []
              }
        }

--------------------------------------------------------------------------------
-- Contextify
--------------------------------------------------------------------------------

-- | @sexpsByModule@ combines in-packages forms and forms following into named
-- module pairs that are suitable for passing into fullyContextify for
-- processing.
--
-- If the list of Sexps passed to the function does not begin with an in-package
-- form then the first argument is used as the name of the module for all Sexps
-- before the first in-package form
--
-- in-package forms that are followed by other in-package forms are ignored.
sexpsByModule :: NameSymbol.T -> [Sexp.T] -> NonEmpty (NameSymbol.T, [Sexp.T])
sexpsByModule initialModule sexps =
  let (sexpsByModule, remaining) = foldr f ([], []) sexps
      singleton a = a :| []
   in case remaining of
        [] ->
          nonEmpty sexpsByModule
            |> maybe ((initialModule, []) |> singleton) identity
        prefix -> (initialModule, prefix) |> singleton
  where
    f sexp (sexpsByModule, sexpsInCurrentModule) =
      Sexp.deserialize sexp
        |> maybe (sexpsByModule, sexp : sexpsInCurrentModule) g
      where
        g (Structure.InPackage name) = case sexpsInCurrentModule of
          [] -> (sexpsByModule, [])
          xs -> ((name, xs) : sexpsByModule, [])

-- | @inContextSexps@ extracts sexps that require further processing after
-- contextify transforms them to name symbols that reference the corresponding
-- item in the context.
inContextSexps :: (NameSymbol.T, [Sexp.T]) -> [Pipeline.EnvOrSexp]
inContextSexps (moduleName, sexps) = inPackage : (sexps >>= f)
  where
    inPackage =
      moduleName
        |> Context.addTopName
        |> Structure.InPackage
        |> Sexp.serialize
        |> Pipeline.Sexp
    f sexp
      | (Just defun) <-
          sexp |> Sexp.deserialize @Structure.DefunSigMatch =
        processDefun defun
          |> maybe (error "malformed defun") pure
      | (Just typ) <-
          sexp |> Sexp.deserialize @Structure.Type =
        processType typ
          |> maybe (error "malformed type") identity
      | otherwise = []

    processDefun defun = getName (defun ^. name)

    processType typ = do
      name <- getName (Sexp.car $ typ ^. nameAndSig)
      (name : (getSumConsNames $ typ ^. body)) |> pure

    getSumConsNames body = Sexp.toList body |> maybe [] g
      where
        g s = traverse (getName . Sexp.car) s |> maybe [] identity

    getName e = Sexp.atomFromT e >>= g
      where
        g (Sexp.A {atomName}) =
          (Pipeline.InContext $ moduleName <> atomName) |> Just
        g _ = Nothing

-- | @initContextPass@ initializes the Context with input Sexps
initContextPass :: Step.Named
initContextPass =
  initContextTrans
    |> Step.T
    |> Step.namePass "Context.initContext"

initContextTrans ::
  Pipeline.CIn -> IO (Pipeline.COut Pipeline.WorkingEnv)
initContextTrans cin = do
  let wenv = cin ^. languageData
      sexps = (wenv ^. currentExp) >>= f |> nonEmpty
      meta = cin ^. surroundingData . metaInfo
  case sexps of
    Nothing -> Pipeline.Success {_meta = meta, _result = wenv} |> pure
    Just toProcess -> initContext meta (wenv ^. context) toProcess
  where
    f (Pipeline.InContext _) = []
    f (Pipeline.Sexp sexp) = [sexp]

initContext ::
  Meta.T ->
  Context.T ->
  NonEmpty Sexp.T ->
  IO (Pipeline.COut Pipeline.WorkingEnv)
initContext meta ctx sexps = do
  context <- fullyContextify ctx moduleSexps
  case context of
    -- TODO: Calls to fullyContextify and below should use MinimialIO so
    -- we get proper Feedback and Tracing
    Left err -> Pipeline.Failure (errMeta err) Nothing |> pure
    Right ctx ->
      Pipeline.WorkingEnv ctxSexps ctx
        |> Pipeline.Success meta
        |> pure
  where
    moduleSexps =
      NonEmpty.toList sexps
        |> sexpsByModule (Context.currentName ctx)
    ctxSexps = (NonEmpty.toList moduleSexps) >>= inContextSexps
    errMeta err = over Meta.feedback (updateFeedback err) meta
    updateFeedback err f =
      Feedback.addMessageNoEff Feedback.Error (errSexp err) f
    errSexp err = Sexp.serialize @Contextify.ResolveErr err

fullyContextify ::
  Context.T ->
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO (Either Contextify.ResolveErr Context.T)
fullyContextify ctx ts = do
  cont <- contextify ctx ts
  case cont of
    Left e -> pure $ Left $ Contextify.Path e
    Right x -> do
      addedOpens <- uncurry ResolveOpen.run x
      case addedOpens of
        Left e -> pure $ Left $ Contextify.Resolve e
        Right x ->
          pure $ Right x

contextify ::
  Context.T ->
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO (Contextify.PathError (Context.T, [ResolveOpen.PreQualified]))
contextify ctx t = do
  runM $
    foldM Contextify.resolveOpens (ctx, []) (addTop <$> t)

addTop :: Bifunctor p => p NameSymbol.T c -> p NameSymbol.T c
addTop = first (NameSymbol.cons Context.topLevelName)

runM :: Contextify.M a -> IO (Either Context.PathError a)
runM (Contextify.M a) = runExceptT a

traverseOnDeserialized ::
  (Monad m, Sexp.Serialize a) =>
  (Sexp.Atom a -> (Sexp.B a -> m (Sexp.B a)) -> m (Sexp.B a)) ->
  Sexp.T ->
  m Sexp.T
traverseOnDeserialized f sexp =
  Sexp.withSerialization sexp (`Sexp.traverseOnAtoms` f)
