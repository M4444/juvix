{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Golden where

import qualified Data.ByteString as ByteString (readFile)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Backends.Interpreter as Interpreter
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import qualified Juvix.Core.HR.Types as HR
import Juvix.Library
  ( Char,
    Either (..),
    Eq,
    FilePath,
    IO,
    MonadIO (liftIO),
    Print (putStrLn),
    Read,
    Semigroup ((<>)),
    Show,
    Text,
    Traversable (sequence),
    identity,
    print,
    readFile,
    ($),
    (.),
    (<$>),
    (=<<),
    (>>=),
  )
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Library.Test.Golden
  ( NoQuotesText,
    discoverAndRunGoldenTests,
    expectFailure,
    expectSuccess',
    getGolden,
    pShowDefault,
    pShowText,
  )
import Juvix.Pipeline (Pipeline)
import qualified Juvix.Pipeline as Pipeline
import Test.Tasty (TestTree, testGroup)
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
  testGroup "Interpreter golden tests"
    <$> sequence
      [ typecheckTests,
        evalTests,
        hrTests,
        irTests,
        erasedTests
      ]

evalTests :: IO TestTree
evalTests =
  testGroup "Interpreter evaluation"
    <$> sequence
      [ compileTestPos "test/examples/positive/interpreter",
        compileTestNeg "test/examples/negative/interpreter/eval"
      ]
  where
    compileTestPos =
      interpreterGoldenTests ".interpreter" (expectSuccess' pShowText . compile)
    compileTestNeg =
      interpreterGoldenTests ".interpreter" (expectFailure . compile)
    compile file =
      Interpreter.compileProgram . ErasedAnn.toRaw =<< typecheckFile file

typecheckFile ::
  FilePath ->
  Feedback.FeedbackT [] String IO (ErasedAnn.AnnTermT Interpreter.PrimTy Interpreter.RawPrimVal)
typecheckFile file = do
  contract <- liftIO $ readFile file
  context <- Pipeline.parse Interpreter.BInterpreter contract
  Pipeline.typecheck @Interpreter.BInterpreter context

typecheckTests :: IO TestTree
typecheckTests =
  testGroup "Interpreter typecheck"
    <$> sequence
      [ typecheckTestPos "test/examples/positive/interpreter",
        typecheckTestNeg "test/examples/negative/interpreter/typecheck"
      ]
  where
    typecheckTestPos =
      interpreterGoldenTests ".typecheck" (expectSuccess' pShowDefault . typecheckFile)
    typecheckTestNeg =
      interpreterGoldenTests ".typecheck" (expectFailure . typecheckFile)

interpreterGoldenTests ::
  String ->
  (FilePath -> IO NoQuotesText) ->
  FilePath ->
  IO TestTree
interpreterGoldenTests ext action (withJuvixRootPath -> p) =
  discoverAndRunGoldenTests identity ext getGolden action p

compile :: FilePath -> Feedback.FeedbackT [] String IO Text
compile file = Interpreter.compileProgram . ErasedAnn.toRaw =<< typecheckFile file

hrTests :: IO TestTree
hrTests =
  testGroup "Interpreter HR"
    <$> sequence
      [ hrTestsPos "test/examples/positive/interpreter",
        hrTestsNeg "test/examples/negative/interpreter/hr"
      ]
  where
    hrTestsPos =
      interpreterGoldenTests ".hr" (expectSuccess' pShowDefault . pipelineToHR)
    hrTestsNeg =
      interpreterGoldenTests ".hr" (expectFailure . pipelineToHR)

pipelineToHR file =
  liftIO (readFile file)
    >>= Pipeline.toML Interpreter.BInterpreter
    >>= Pipeline.toSexp Interpreter.BInterpreter
    >>= Pipeline.toHR Interpreter.interpreter

pipelineToIR file = pipelineToHR file >>= Pipeline.toIR

irTests :: IO TestTree
irTests =
  testGroup "Interpreter IR"
    <$> sequence
      [ irTestsPos "test/examples/positive/interpreter",
        irTestsNeg "test/examples/negative/interpreter/ir"
      ]
  where
    irTestsPos =
      interpreterGoldenTests ".ir" (expectSuccess' pShowDefault . pipelineToHR)
    irTestsNeg =
      interpreterGoldenTests ".ir" (expectFailure . pipelineToHR)

erasedTests :: IO TestTree
erasedTests =
  testGroup "Interpreter Erased"
    <$> sequence
      [ erasedTestsPos "test/examples/positive/interpreter",
        erasedTestsNeg "test/examples/negative/interpreter/erased"
      ]
  where
    erasedTestsPos =
      interpreterGoldenTests ".erased" (expectSuccess' pShowDefault . pipelineToHR)
    erasedTestsNeg =
      interpreterGoldenTests ".erased" (expectFailure . pipelineToHR)
    toErased file =
      liftIO (readFile file)
        >>= Pipeline.toML Interpreter.BInterpreter
        >>= Pipeline.toSexp Interpreter.BInterpreter
        >>= Pipeline.toHR Interpreter.interpreter
        >>= Pipeline.toIR
        >>= Pipeline.toErased Interpreter.interpreter
