module Test.Context.Helpers where

import Control.Lens (view, (^.))
import Juvix.BerlinPasses.Contextify (contextify)
import qualified Juvix.BerlinPipeline.Env as Pipeline.Env
import qualified Juvix.BerlinPipeline.Feedback as BerlinPipeline.Feedback
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as Contextify
import qualified Juvix.Contextify.ToContext.Types as Contextify
import qualified Juvix.Desugar.Env as Desugar
import qualified Juvix.Desugar.Env as Desugar.Env
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Parsing.Parser as Parser
import qualified Juvix.Parsing.Types.Base as Parsing
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Transition as Structure
import qualified Juvix.Translate.Pipeline.TopLevel as TopLevel
import Text.Pretty.Simple (pShowNoColor)

----------------------------------------------------------------------
-- Give me sexp terms helpers
----------------------------------------------------------------------

contextualizeFoo ::
  ByteString ->
  IO
    ( Either
        Context.PathError
        (Context.T, [Contextify.PreQualified])
    )
contextualizeFoo byte = do
  ctx <- Context.empty "JU-USER"
  sexp <- parseDesugarSexp byte
  contextify ctx (("Foo", sexp) :| [])

parseDesugarSexp :: ByteString -> IO [Sexp.T]
parseDesugarSexp = desugarLisp . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| TopLevel.transTopLevel

ignoreHeader :: Either a (Parsing.Header topLevel) -> [topLevel]
ignoreHeader (Right (Parsing.NoHeader xs)) = xs
ignoreHeader _ = panic "not no header"

desugarLisp :: [Sexp.T] -> IO [Sexp.T]
desugarLisp xs =
  runPipelineToStep "Context.initContext" [("Juvix-User", xs)]
    >>| view Pipeline.currentExp
    >>| sexps
  where
    sexps xs = [s | (Pipeline.Sexp s) <- xs]

runSexpPipeline ::
  Pipeline.Env.EnvS () ->
  [(NameSymbol.T, [Sexp.T])] ->
  IO Pipeline.WorkingEnv
runSexpPipeline pipeline x = do
  Pipeline.CIn languageData surrounding <- runSexpPipelineEnv pipeline x

  let feedback = surrounding ^. Pipeline.metaInfo . Meta.feedback
      errors = BerlinPipeline.Feedback.getErrors feedback

  case errors of
    [] -> languageData |> pure
    es -> Feedback.fail . toS . pShowNoColor $ es

runSexpPipelineEnv ::
  Pipeline.Env.EnvS () ->
  [(NameSymbol.T, [Sexp.T])] ->
  IO Pipeline.CIn
runSexpPipelineEnv pipeline x =
  do
    let workingEnv =
          x >>= mergeTopLevel
            >>| Pipeline.Sexp
            |> Pipeline.WorkingEnv
        defaultNs = fromMaybe "JU-USER" (headMay x >>| fst)
        startingEnv =
          (Context.empty defaultNs :: IO Context.T)
            >>| workingEnv

    startingEnv
    >>= Pipeline.Env.run pipeline
      . Pipeline.emptyInput
  where
    inPackage name = Structure.InPackage name |> Sexp.serialize
    mergeTopLevel (name, exps) = [inPackage name] <> exps

runPipelineToStep ::
  NameSymbol.T ->
  [(NameSymbol.T, [Sexp.T])] ->
  IO Pipeline.WorkingEnv
runPipelineToStep step = runSexpPipeline pipeline
  where
    pipeline = Pipeline.Env.stopAt step >> Desugar.Env.eval

emptyContextify names = do
  ctx <- Context.empty "JU-USER"
  contextify ctx names
