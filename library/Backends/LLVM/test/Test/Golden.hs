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

--------------------------------------------------------------------------------
-- Parse contracts (Golden tests)
--------------------------------------------------------------------------------

juvixRootPath :: FilePath
juvixRootPath = "../../../"

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

top :: IO TestTree
top =
  testGroup "LLVM golden tests"
    <$> sequence
      [ typecheckTests,
        compileTests,
        hrTests,
        irTests,
        erasedTests
      ]

compileTests :: IO TestTree
compileTests =
  testGroup "LLVM compile"
    <$> sequence
      [ compileTestPos "test/examples/positive/llvm",
        compileTestNeg "test/examples/negative/llvm/compile"
      ]
  where
    compileTestPos = llvmGoldenTests ".llvm" (expectSuccess' pShowText . compile)
    compileTestNeg = llvmGoldenTests ".llvm" (expectFailure . compile)
    compile file = LLVM.compileProgram . ErasedAnn.toRaw =<< typecheckFile file

typecheckTests :: IO TestTree
typecheckTests =
  testGroup "LLVM typecheck"
    <$> sequence
      [ typecheckTestPos "test/examples/positive/llvm",
        typecheckTestNeg "test/examples/negative/llvm/typecheck"
      ]
  where
    typecheckTestPos = llvmGoldenTests ".typecheck" (expectSuccess' pShowDefault . typecheckFile)
    typecheckTestNeg = llvmGoldenTests ".typecheck" (expectFailure . typecheckFile)

typecheckFile ::
  FilePath ->
  Feedback.FeedbackT [] String IO (ErasedAnn.AnnTermT LLVM.PrimTy LLVM.RawPrimVal)
typecheckFile file = do
  contract <- liftIO $ readFile file
  context <- Pipeline.parse LLVM.BLLVM contract
  Pipeline.typecheck @LLVM.BLLVM context

hrTests :: IO TestTree
hrTests =
  testGroup "LLVM HR"
    <$> sequence
      [ hrTestsPos "test/examples/positive/llvm",
        hrTestsNeg "test/examples/negative/llvm/hr"
      ]
  where
    hrTestsPos = llvmGoldenTests ".hr" (expectSuccess' pShowDefault . pipelineToHR)
    hrTestsNeg = llvmGoldenTests ".hr" (expectFailure . pipelineToHR)

pipelineToHR file =
  do
    liftIO (readFile file)
    >>= Pipeline.toML LLVM.BLLVM
    >>= Pipeline.toSexp LLVM.BLLVM
    >>= Pipeline.toHR LLVM.llvm

pipelineToIR file = pipelineToHR file >>= Pipeline.toIR

irTests :: IO TestTree
irTests =
  testGroup "LLVM IR"
    <$> sequence
      [ hrTestsPos "test/examples/positive/llvm",
        hrTestsNeg "test/examples/negative/llvm/ir"
      ]
  where
    hrTestsPos = llvmGoldenTests ".ir" (expectSuccess' pShowDefault . pipelineToIR)
    hrTestsNeg = llvmGoldenTests ".ir" (expectFailure . pipelineToIR)

erasedTests :: IO TestTree
erasedTests =
  testGroup "LLVM Erased"
    <$> sequence
      [ hrTestsPos "test/examples/positive/llvm",
        hrTestsNeg "test/examples/negative/llvm/erased"
      ]
  where
    hrTestsPos = llvmGoldenTests ".erased" (expectSuccess' pShowDefault . toErased)
    hrTestsNeg = llvmGoldenTests ".erased" (expectFailure . toErased)
    toErased file =
      do
        liftIO (readFile file)
        >>= Pipeline.toML LLVM.BLLVM
        >>= Pipeline.toSexp LLVM.BLLVM
        >>= Pipeline.toHR LLVM.llvm
        >>= Pipeline.toIR
        >>= Pipeline.toErased LLVM.llvm

llvmGoldenTests ::
  [Char] ->
  (FilePath -> IO NoQuotesText) ->
  FilePath ->
  IO TestTree
llvmGoldenTests ext action (withJuvixRootPath -> p) = discoverAndRunGoldenTests identity ext getGolden action p

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
