{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | @Juvix.Library.Test.Golden@ defines testing functionality for golden
--   style tests
-- - Golden tests revolve around testing files we have saved on
--   disk. Namely we wish to take that file and do some transformation
--   and save the result to compare it for regression testing.
--
-- - There are many useful sub components of this module
--
-- - The =Compact= tag to a few of the functions represents golden
--   test functions that display the results in different ways. Often
--   we use the =Compact= variant for S-expression showing as it's
--   much clearer to see what the expressions mean.
--
-- * NoQuotesText
-- This structure allows us to have golden tests that are based around
-- show instances instead of normal read instances.
module Juvix.Library.Test.Golden
  ( NoQuotesText (..),

    -- * Testing functionalities with the normal show and with no colors
    prettyAction,
    compareGolden,
    mkGoldenTest,
    runGoldenTests,
    discoverAndRunGoldenTests,

    -- *
    getGolden,
    expectSuccess,
    expectSuccess',
    expectFailure,

    -- * Running tests expecting failure
    defaultMainFail,
    runAll,

    -- * Priting options
    PPrinter (..),
    pShowDefault,
    pShowText,
    pShowCompact,
  )
where

import qualified Control.Exception as Except
import qualified Data.ByteString as ByteString (writeFile)
import Data.String (String)
import qualified Data.Text as Text
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath as FP
import Test.Tasty
import qualified Test.Tasty.Ingredients as Ingredients
import qualified Test.Tasty.Ingredients.Basic as Ingredients.Basic
import qualified Test.Tasty.Silver as T
import qualified Test.Tasty.Silver.Advanced as T
import qualified Text.Pretty.Simple as Pretty
import Text.Read (Read (..))
import qualified Prelude (error, show)
import Debug.Pretty.Simple (pTrace)

type FileExtension = String

-- | Text that doesn't duplicate quotes when applying `show` onto it
newtype NoQuotesText = NoQuotesText {unText :: Text}

emptyNoQuotes :: NoQuotesText
emptyNoQuotes = NoQuotesText ""

instance Show NoQuotesText where
  show (NoQuotesText t) = toS t

instance Read NoQuotesText where
  readsPrec _ s = [(NoQuotesText $ toS s, "")]

instance Eq NoQuotesText where
  (NoQuotesText t1) == (NoQuotesText t2) = t1 == t2

prettyAction ::
  (Monad m, Show a) =>
  (a -> NoQuotesText) ->
  (FilePath -> m a) ->
  FilePath ->
  m NoQuotesText
prettyAction prettifier action filepath = prettifier <$> action filepath

getGolden :: (Read a, Show a) => FilePath -> IO (Maybe a)
getGolden file = do
  createDirectoryIfMissing True $ FP.takeDirectory file
  maybeBS <- T.readFileMaybe file

  return $ do
    bs <- maybeBS
    readMaybe $ Text.unpack $ decodeUtf8 bs

compareGoldenPretty ::
  (Eq a, Show a) => (a -> NoQuotesText) -> a -> a -> T.GDiff
compareGoldenPretty prettyPrinter golden upcoming
  | upcoming == golden =
    T.Equal
  | otherwise =
    T.DiffText
      { T.gReason =
          Just $
            "Output doesn't match golden file."
              <> "The new result is \n"
              <> show (prettyPrinter upcoming)
              <> "\n but the expected result is \n"
              <> show (prettyPrinter golden),
        T.gActual = resultToText upcoming,
        T.gExpected = resultToText golden
      }
  where
    resultToText :: Show a => a -> Text
    resultToText = Text.pack . show

compareGolden :: (Eq a, Show a) => a -> a -> T.GDiff
compareGolden = compareGoldenPretty pShowDefault

-- | Run golden tests over a list of files
runGoldenTests ::
  (Show a, Eq a) =>
  -- | pretty show function for output code
  (a -> NoQuotesText) ->
  -- | the output file extension
  FileExtension ->
  -- | get golden
  (FilePath -> IO (Maybe a)) ->
  -- | action
  (FilePath -> IO a) ->
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  -- | the names of the files to test
  [FilePath] ->
  TestTree
runGoldenTests prettyShow ext_out getGolden action path filenames =
  testGroup path $ map (mkGoldenTest prettyShow getGolden action ext_out)  ((path FP.</>) <$> filenames)

-- | Discover files from an extension and a path and run golden tests
discoverAndRunGoldenTests ::
  (Show a, Eq a) =>
  -- | pretty show function for output code
  (a -> NoQuotesText) ->
  -- | the output file extension
  FileExtension ->
  -- | get golden
  (FilePath -> IO (Maybe a)) ->
  -- | action
  (FilePath -> IO a) ->
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO TestTree
discoverAndRunGoldenTests prettyShow extOut getGolden action path =
  testGroup path . map (mkGoldenTest prettyShow getGolden action extOut) <$> T.findByExtension [".ju"] path

toGolden :: (ConvertText a Text, ConvertText Text c) => a -> c
toGolden = toS . Text.replace "examples" "examples-golden" . toS

-- | Make a single golden test
mkGoldenTest ::
  (Show a, Eq a) =>
  -- | pretty show function for output code
  (a -> NoQuotesText) ->
  -- | get golden
  (FilePath -> IO (Maybe a)) ->
  -- | action
  (FilePath -> IO a) ->
  -- | the extension of the outfile, e.g. @".parsed"@
  FileExtension ->
  -- | the file path of the input file
  FilePath ->
  TestTree
mkGoldenTest prettyShow getGolden action ext pathToFile = 
  pTrace pathToFile $ T.goldenTest1
    outFilename
    (getGolden outfile)
    (action pathToFile)
    compareGolden
    -- show the golden/actual value
    (T.ShowText . unText . prettyShow)
    createOutput
  where
    directory = FP.dropFileName pathToFile
    goldenBase = FP.takeBaseName pathToFile
    outFilename = FP.replaceExtension (FP.takeFileName pathToFile) ext
    outfile = toGolden directory FP.</> goldenBase FP.</> outFilename
    createOutput =
      ByteString.writeFile outfile
        . (encodeUtf8 . unText . prettyShow)

--------------------------------------------------------------------------------
-- Expectations
--------------------------------------------------------------------------------

-- | Expect a successful computation
expectSuccess :: (Monad m, Show (app msg)) => Feedback.FeedbackT app msg m b -> m b
expectSuccess v = do
  feedback <- Feedback.runFeedbackT v
  case feedback of
    Feedback.Success _msgs r -> pure r
    Feedback.Fail msgs -> panic $ "Expected success but failed: " <> show msgs

-- | Expect a successful computation. Prettify the output
expectSuccess' :: (Monad m, Show (app msg), Show b, Eq b) => (b -> NoQuotesText) -> Feedback.FeedbackT app msg m b -> m NoQuotesText
expectSuccess' prettifier v = prettifier <$> expectSuccess v

-- | Expect that computation fails
expectFailure :: (Monad m, Show a, Show (app msg)) => Feedback.FeedbackT app msg m a -> m NoQuotesText
expectFailure v = do
  feedback <- Feedback.runFeedbackT v
  case feedback of
    Feedback.Success _msgs r -> panic $ "Expected failure but succeeded with: " <> show r
    Feedback.Fail _msgs -> pure emptyNoQuotes

--------------------------------------------------------------------------------
-- Pretty Printers
--------------------------------------------------------------------------------

data PPrinter = PDefault | PText | PCompact
  deriving (Show, Eq)

pShowText :: Text -> NoQuotesText
pShowText = NoQuotesText

pShowDefault :: Show a => a -> NoQuotesText
pShowDefault = NoQuotesText . toS . Pretty.pShowNoColor

pShowCompact :: Show a => a -> NoQuotesText
pShowCompact =
  NoQuotesText . toS
    . Pretty.pShowOpt
      ( Pretty.defaultOutputOptionsNoColor
          { Pretty.outputOptionsCompactParens = True,
            Pretty.outputOptionsCompact = True
          }
      )

--------------------------------------------------------------------------------
-- Expecting Failure runners
--------------------------------------------------------------------------------

-- | @defaultMainFail@ is like Tasty @defaultMain@ but will fail if all
-- tests succeed. Thus we give success if there exists a failure case
defaultMainFail :: TestTree -> IO ()
defaultMainFail =
  defaultMainWithIngredients [Ingredients.Basic.listingTests, consoleTestReporterFailure]
  where
    consoleTestReporterFailure =
      case Ingredients.Basic.consoleTestReporter of
        Ingredients.TestReporter opts fun ->
          Ingredients.TestReporter
            opts
            ( \opt tree ->
                case fun opt tree of
                  Just f ->
                    Just $
                      \status -> do
                        timeF <- f status
                        pure $ \time -> do
                          ans <- timeF time
                          pure (not ans)
                  Nothing ->
                    Nothing
            )
        Ingredients.TestManager _ _ -> Prelude.error "impossible"

-- | @runAll@ allows the user to run many defaultMains. This is useful
-- when the user wants to run @defaultMainFail@ along with @defaultMain@
runAll :: Foldable t => t (IO ()) -> IO b
runAll listOfMains = do
  traverse_ ignoreSuccess listOfMains
  exitSuccess
  where
    ignoreSuccess run = do
      tried <- Except.try run :: IO (Either Except.SomeException ())
      case tried of
        Right _ -> pure ()
        Left except ->
          let exit = fromException except :: Maybe ExitCode
           in case exit of
                Just e@(ExitFailure _) -> exitWith e
                Just (ExitSuccess) -> pure ()
                Nothing -> pure ()
