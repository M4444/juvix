{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Backends.LLVM as LLVM
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.HR.Types as HR
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Library.Test.Golden
import Juvix.Pipeline (Pipeline)
import qualified Juvix.Pipeline as Pipeline
import Test.Tasty
import Prelude (String)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

juvixRootPath :: FilePath
juvixRootPath = "../../.."

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath </> p

testFilesPos :: FilePath
testFilesPos = "test/examples/positive/llvm"

testFilesNeg :: FilePath
testFilesNeg = "test/examples/negative/llvm"

top :: TestTree
top =
  testGroup "LLVM golden tests"
      [ typecheckTests,
        compileTests,
        hrTests,
        irTests,
        erasedTests
      ]

compileTests :: TestTree
compileTests =
  testGroup "LLVM compile"
      [ compileTestPos testFilesPos,
        compileTestNeg $ testFilesNeg </> "compile"
      ]
  where
    compileTestPos = llvmGoldenTestsPos ".llvm" (expectSuccess' pShowText . compile)
    compileTestNeg = llvmGoldenTestsNeg [] ".llvm" (expectFailure . compile)
    compile file = LLVM.compileProgram . ErasedAnn.toRaw =<< typecheckFile file

typecheckTests :: TestTree
typecheckTests =
  testGroup "LLVM typecheck"
      [ typecheckTestPos testFilesPos,
        typecheckTestNeg $ testFilesNeg </> "typecheck"
      ]
  where
    typecheckTestPos = llvmGoldenTestsPos ".typecheck" (expectSuccess' pShowDefault . typecheckFile)
    typecheckTestNeg = llvmGoldenTestsNeg fileNamesTypecheckNeg ".typecheck" (expectFailure . typecheckFile)

typecheckFile ::
  FilePath ->
  Feedback.FeedbackT [] String IO (ErasedAnn.AnnTermT LLVM.PrimTy LLVM.RawPrimVal)
typecheckFile file = do
  contract <- liftIO $ readFile file
  context <- Pipeline.parse LLVM.BLLVM contract
  Pipeline.typecheck @LLVM.BLLVM context

hrTests :: TestTree
hrTests =
  testGroup "LLVM HR"
      [ hrTestsPos testFilesPos,
        hrTestsNeg $ testFilesNeg </> "hr"
      ]
  where
    hrTestsPos = llvmGoldenTestsPos ".hr" (expectSuccess' pShowDefault . pipelineToHR)
    hrTestsNeg = llvmGoldenTestsNeg [] ".hr" (expectFailure . pipelineToHR)

pipelineToHR file =
  do
    liftIO (readFile file)
    >>= Pipeline.toML LLVM.BLLVM
    >>= Pipeline.toSexp LLVM.BLLVM
    >>= Pipeline.toHR LLVM.llvm

pipelineToIR file = pipelineToHR file >>= Pipeline.toIR

irTests :: TestTree
irTests =
  testGroup "LLVM IR"
      [ hrTestsPos testFilesPos,
        hrTestsNeg $ testFilesNeg </> "ir"
      ]
  where
    hrTestsPos = llvmGoldenTestsPos ".ir" (expectSuccess' pShowDefault . pipelineToIR)
    hrTestsNeg = llvmGoldenTestsNeg [] ".ir" (expectFailure . pipelineToIR)

erasedTests :: TestTree
erasedTests =
  testGroup "LLVM Erased"
      [ hrTestsPos testFilesPos,
        hrTestsNeg $ testFilesNeg </> "erased"
      ]
  where
    hrTestsPos = llvmGoldenTestsPos ".erased" (expectSuccess' pShowDefault . toErased)
    hrTestsNeg = llvmGoldenTestsNeg [] ".erased" (expectFailure . toErased)
    toErased file =
      do
        liftIO (readFile file)
        >>= Pipeline.toML LLVM.BLLVM
        >>= Pipeline.toSexp LLVM.BLLVM
        >>= Pipeline.toHR LLVM.llvm
        >>= Pipeline.toIR
        >>= Pipeline.toErased LLVM.llvm

fileNamesPos :: [FilePath]
fileNamesPos =
  [ "Dependencies/D.ju"
  , "main/MainConst.ju"
  , "main/MainMultiArgs.ju"
  , "Const.ju"
  , "ConstAddPrim.ju"
  , "HelloWorld.ju"
  , "Instructions.ju"
  , "MainApply.ju"
  , "MainLambda.ju"
  , "Nested.ju"
  , "StringsWithClosures.ju"
  ]

fileNamesTypecheckNeg :: [FilePath]
fileNamesTypecheckNeg = 
  [ "typecheck/Datatypes/TooManyArguments.ju"
  ]

llvmGoldenTestsPos ::
  [Char] ->
  (FilePath -> IO NoQuotesText) ->
  FilePath ->
  TestTree
llvmGoldenTestsPos = llvmGoldenTests fileNamesPos

llvmGoldenTestsNeg ::
  [FilePath] ->
  [Char] ->
  (FilePath -> IO NoQuotesText) ->
  FilePath ->
  TestTree
llvmGoldenTestsNeg = llvmGoldenTests

llvmGoldenTests ::
  [FilePath] ->
  [Char] ->
  (FilePath -> IO NoQuotesText) ->
  FilePath ->
  TestTree
llvmGoldenTests fileNames ext action (withJuvixRootPath -> llvmRootPath) = 
  runGoldenTests identity ext getGolden action llvmRootPath fileNames

compile :: FilePath -> Feedback.FeedbackT [] String IO Text
compile file = LLVM.compileProgram . ErasedAnn.toRaw =<< typecheckFile file

toEither :: Feedback.Feedback app msg b -> Either (app msg) (app msg, b)
toEither (Feedback.Success app a) = Right (app, a)
toEither (Feedback.Fail failure) = Left failure

-- Running by hand example
printCompile :: FilePath -> IO ()
printCompile filePath = do
  x <- Feedback.runFeedbackT (compile filePath)
  case toEither x of
    Right (_, str) ->
      putStrLn str
    Left err -> print err
