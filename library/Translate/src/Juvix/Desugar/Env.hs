module Juvix.Desugar.Env where

import Control.Lens hiding ((|>))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.BerlinPipeline.Automation as Automation
import qualified Juvix.BerlinPipeline.CircularList as CircularList
import qualified Juvix.BerlinPipeline.Env as Pipeline.Env
import qualified Juvix.BerlinPipeline.Feedback as Feedback
import Juvix.BerlinPipeline.Lens
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.BerlinPipeline.Step as Step
import qualified Juvix.Closure as Closure
import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.Passes as Passes
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as ResolveOpen
import qualified Juvix.Contextify.ToContext.Types as Contextify
import Juvix.Library hiding (trace)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp
import Juvix.Sexp.Structure.Lens
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure
import Prelude (error)

--------------------------------------------------------------------------------
-- Environment
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

data Error = MalformedData Text
  deriving (Generic, Show, Eq)

instance Sexp.DefaultOptions Error

instance Sexp.Serialize Error

runEnv :: MinimalMIO a -> Meta.T -> IO (Either Sexp.T a, Env)
runEnv (MinIO a) (Meta.T {_trace, _feedback}) =
  runStateT
    (runExceptT a)
    Env
      { trace = _trace,
        feedback = _feedback,
        closure = Closure.empty
      }

-- TODO ∷ update to use feedback before throwing. (Sadly not finished)
throw :: (Feedback.Eff m, HasThrow "error" Sexp.T m) => Error -> m a2
throw err = do
  Feedback.error (Sexp.serialize err)
  Juvix.Library.throw @"error" (Sexp.serialize err)

-- Do not use externally
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

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

Right sexp =
  Sexp.parse
    "(defun foo (x) (:cond (pred-1 (:cond (pred-1 result-1) (pred-n result-n))) (pred-n result-n)))"

Right secondSexp = Sexp.parse "(defun foo (x) (+ x 1))"

startingEnv :: IO Pipeline.WorkingEnv
startingEnv =
  (Context.empty "JU-USER" :: IO (Context.T Sexp.T Sexp.T Sexp.T))
    >>| Pipeline.WorkingEnv [Pipeline.Sexp sexp, Pipeline.Sexp secondSexp]

exampleMeta :: IO Meta.T
exampleMeta = do
  Pipeline.CIn languageData surroudning <-
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

exampleIndexing :: IO (Maybe Error)
exampleIndexing = do
  myValue <- exampleMeta
  Feedback.contentsAt 0 (myValue ^. Meta.feedback)
    |> Sexp.deserialize @Error
    |> pure

eval :: Pipeline.Env.EnvS ()
eval = do
  Pipeline.Env.registerStep (CircularList.init headerPass)
  Pipeline.Env.registerAfterEachStep inPackageTrans
  Pipeline.Env.registerStep (CircularList.init condPass)

condPass :: Step.Named
condPass =
  (Trace.withScope "Desugar.cond-runner" [] . Automation.simplify condTrans)
    |> Automation.runSimplifiedPass
    |> Step.T
    |> Step.namePass "Desugar.cond-to-if"

condTrans :: Automation.SimplifiedPassArgument -> MinimalMIO Automation.Job
condTrans simplify = do
  Trace.withScope "Desugar.condTrans" [show (simplify ^. current)] $ do
    condTransform (simplify ^. current)
      >>| (\transformed -> Automation.ProcessNoEnv transformed [])
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
    condToIf _ _ = Juvix.Desugar.Env.throw $ MalformedData "cond is in an invalid format"
    --
    generation predAns acc =
      Structure.If (predAns ^. predicate) (predAns ^. answer) acc
        |> Structure.fromIf

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
-- Header
--------------------------------------------------------------------------------

injectCurrentPackageContext :: Pipeline.CIn -> Pipeline.CIn
injectCurrentPackageContext cin =
  over (languageData . currentExp) f cin
  where
    f =
      (cin ^. languageData . context)
        |> Context.currentName
        |> NameSymbol.cons Context.topLevelName
        |> Structure.InPackage
        |> Sexp.serialize
        |> Pipeline.Sexp
        |> (:)

headerPass :: Step.Named
headerPass =
  (Trace.withScope "Desugar.header-runner" [] . Automation.simplify headerTrans)
    |> (\f -> Automation.runSimplifiedPass f . injectCurrentPackageContext)
    |> Step.T
    |> Step.namePass "Desugar.header"

headerTrans :: Automation.SimplifiedPassArgument -> MinimalMIO Automation.Job
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
        (Sexp.serialize (Structure.InPackage (Context.topLevelName <> name)), sexps)
          |> pure
      Nothing -> Juvix.Desugar.Env.throw $ MalformedData "header is in an invalid format"

--------------------------------------------------------------------------------
-- Resolve Modules
--------------------------------------------------------------------------------

resolveModuleTransform ::
  ( HasThrow "error" Sexp.T m,
    HasClosure m,
    MonadIO m
  ) =>
  Context.T Sexp.T Sexp.T Sexp.T ->
  Sexp.T ->
  m Sexp.T
resolveModuleTransform ctx =
  Passes.openResolution ctx |> traverseOnDeserialized

--------------------------------------------------------------------------------
-- Infix Form Transformation
--------------------------------------------------------------------------------

infixConversionTransform ::
  ( HasThrow "error" Sexp.T m,
    HasClosure m,
    MonadIO m
  ) =>
  Context.T Sexp.T Sexp.T Sexp.T ->
  Sexp.T ->
  m Sexp.T
infixConversionTransform ctx =
  Passes.infixConversion ctx |> traverseOnDeserialized

--------------------------------------------------------------------------------
-- Record Lookup from Unknown Symbols
--------------------------------------------------------------------------------

unknownSymbolLookupTransform ::
  ( HasThrow "error" Sexp.T m,
    HasClosure m,
    MonadIO m
  ) =>
  Context.T Sexp.T Sexp.T Sexp.T ->
  Sexp.T ->
  m Sexp.T
unknownSymbolLookupTransform ctx =
  Passes.primiveOrSymbol ctx |> traverseOnDeserialized

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
inContextSexps (moduleName, sexps) = sexps >>= f
  where
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

initContext ::
  Context.T Sexp.T Sexp.T Sexp.T ->
  NonEmpty Sexp.T ->
  IO (Pipeline.COut Pipeline.WorkingEnv)
initContext ctx sexps = do
  context <- fullyContextify ctx moduleSexps
  case context of
    -- TODO: Calls to fullyContextify and below should use MinimialIO so
    -- we get proper Feedback and Tracing
    Left err -> Pipeline.Failure Meta.empty Nothing |> pure
    Right ctx ->
      Pipeline.WorkingEnv ctxSexps ctx
        |> Pipeline.Success Meta.empty
        |> pure
  where
    moduleSexps =
      NonEmpty.toList sexps
        |> sexpsByModule (Context.currentName ctx)
    ctxSexps = (NonEmpty.toList moduleSexps) >>= inContextSexps

fullyContextify ::
  Context.T Sexp.T Sexp.T Sexp.T ->
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO (Either Contextify.ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
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
  Context.T Sexp.T Sexp.T Sexp.T ->
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO (Contextify.PathError (Contextify.ContextSexp, [ResolveOpen.PreQualified]))
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