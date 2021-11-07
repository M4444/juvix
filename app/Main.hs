module Main (main) where

------------------------------------------------------------------------------

import qualified Juvix.Backends.LLVM as LLVM
import qualified Juvix.Backends.Michelson as Michelson
import qualified Juvix.Backends.Plonk as Plonk
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Pipeline as Pipeline
import Options
import Options.Applicative
import System.Directory (getCurrentDirectory, getHomeDirectory)
import Text.Pretty.Simple (pPrint)
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Version (infoVersionRepo, progNameVersionTag)
import GitHub (github)
import qualified GitHub
import qualified GitHub.Auth as GitHub
import qualified GitHub.Endpoints.Repos.Contents as GitHub
import System.Directory
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString as BS
import Text.Pretty.Simple
------------------------------------------------------------------------------
-- Run commands
------------------------------------------------------------------------------

run' :: Context -> Options -> Pipeline.Pipeline ()
run' _ (Options cmd _) = do
  case cmd of
    Parse fin backend -> runCmd fin backend Pipeline.parse
    Typecheck fin backend -> case backend of
      LLVM b -> g b
      Michelson b -> g b
      Plonk b -> g b
      where
        g ::
          forall b.
          ( Show (Pipeline.Ty b),
            Show (Pipeline.Val b),
            Pipeline.HasBackend b
          ) =>
          b ->
          Pipeline.Pipeline ()
        g b = runCmd' fin b \b ->
          Pipeline.parse b
            >=> Pipeline.typecheck @b
    Compile fin fout backend ->
      case backend of
        LLVM b -> g b
        Michelson b -> g b
        Plonk b -> g b
      where
        g ::
          forall b.
          Pipeline.HasBackend b =>
          b ->
          Pipeline.Pipeline ()
        g b = runCmd' fin b \b ->
          Pipeline.parse b
            >=> Pipeline.typecheck @b
            >=> Pipeline.compile @b fout
    Version -> do
      infoVersion <- liftIO infoVersionRepo
      liftIO $ putDoc infoVersion
    _ -> Feedback.fail "Not implemented yet."

run :: Context -> Options -> IO ()
run ctx opt = do
  feedback <- Feedback.runFeedbackT $ run' ctx opt
  case feedback of
    Feedback.Success msgs _ -> mapM_ pPrint msgs >> exitSuccess
    Feedback.Fail msgs -> mapM_ pPrint msgs >> exitFailure

runCmd ::
  Show a =>
  FilePath ->
  Backend ->
  (forall b. Pipeline.HasBackend b => b -> Text -> Pipeline.Pipeline a) ->
  Pipeline.Pipeline ()
runCmd fin backend f = case backend of
  LLVM b -> runCmd' fin b f
  Michelson b -> runCmd' fin b f
  Plonk b -> runCmd' fin b f

runCmd' ::
  forall a b.
  (Show a, Pipeline.HasBackend b) =>
  FilePath ->
  b ->
  (forall b. Pipeline.HasBackend b => b -> Text -> Pipeline.Pipeline a) ->
  Pipeline.Pipeline ()
runCmd' fin b f = liftIO (readFile fin) >>= f b >>= liftIO . pPrint


installStdLibs :: IO ()
installStdLibs = do
    getContents "stdlib"
    where
      getJuvixHome = (<> "/.juvix/") <$> getHomeDirectory
      createDir p = do
        d <- getJuvixHome 
        createDirectoryIfMissing True (d <> p) 


      getContents :: Text -> IO ()
      getContents path = do
        stdlibsR <- github (GitHub.OAuth "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") $ GitHub.contentsForR "anoma" "juvix" path Nothing

        case stdlibsR of
          Left err -> pPrint err
          Right (GitHub.ContentDirectory stdlibs) -> do
            createDir (toS path)
            traverse_ (\stdlib -> getContents (GitHub.contentPath $ GitHub.contentItemInfo stdlib)) stdlibs
          Right (GitHub.ContentFile file) -> do
            let content = GitHub.contentFileContent file
            let path = GitHub.contentPath $ GitHub.contentFileInfo file
            localJuvix <- getJuvixHome
            BS.writeFile (localJuvix <> (toS path)) (BS.decodeLenient $ encodeUtf8 content)

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
  pwd <- getCurrentDirectory
  home <- getHomeDirectory
  let ctx = Context pwd home
  progVersion <- progNameVersionTag
  installStdLibs
  let opts = info (options ctx <**> helper) (fullDesc <> headerDoc (Just progVersion))
  run ctx =<< execParser opts
