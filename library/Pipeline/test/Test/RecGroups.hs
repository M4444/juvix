{-# LANGUAGE LiberalTypeSynonyms #-}

module Test.RecGroups where

import Control.Lens hiding ((|>))
import qualified Juvix.BerlinPipeline.Env as Pipeline.Env
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Context as Context
import qualified Juvix.Core.Common.Context.Traverse as Traverse
import qualified Juvix.Desugar.Env as Desugar.Env
import Juvix.Library
import qualified Juvix.Parsing as Parsing
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Pipeline.ToSexp as ToSexp
import qualified Juvix.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

juvixRootPath :: FilePath
juvixRootPath = "../../"

withJuvixRootPath :: FilePath -> FilePath
withJuvixRootPath p = juvixRootPath <> p

withJuvixExamplesPath :: FilePath -> FilePath
withJuvixExamplesPath p = juvixRootPath <> "test/examples/" <> p

top :: T.TestTree
top =
  T.testGroup
    "Rec Groups tests"
    [pipeline, pipelineOpen]

toSexp :: [FilePath] -> IO (Either Pipeline.Error Context.T)
toSexp paths = do
  x <- Parsing.parseFiles paths
  case x of
    Left er -> pure $ Left (Pipeline.ParseErr er)
    Right x ->
      second (fmap ToSexp.transTopLevel) <$> x
        |> Pipeline.runSexpPipelineEnv Desugar.Env.eval
        >>| view (Pipeline.languageData . Pipeline.context)
        >>| Right

pipeline :: T.TestTree
pipeline =
  let correctOrder =
        [ "Rec-Groups-Helper" :| ["ty_"],
          "Rec-Groups-Helper" :| ["ty"],
          "Rec-Groups-Helper" :| ["foo"],
          "Rec-Groups" :| ["main"]
        ]
   in T.testCase
        "multiple modules have correct ordering"
        $ do
          Right c <-
            toSexp
              ( withJuvixExamplesPath
                  <$> [ "to-fix/rec-groups/Rec-Groups.ju",
                        "to-fix/rec-groups/Rec-Groups-Helper.ju"
                      ]
              )
          let recd = Traverse.recGroups c
          fmap (\(x :| []) -> Traverse.name x) recd T.@=? correctOrder

pipelineOpen :: T.TestTree
pipelineOpen =
  let correctOrder =
        [ "A" :| ["bar"],
          "B" :| ["fi"],
          "C" :| ["si"],
          "D" :| ["fi"],
          "D" :| ["main"]
        ]
   in T.testCase
        "multiple modules have correct ordering abcd"
        $ do
          Right c <-
            toSexp
              ( withJuvixExamplesPath
                  <$> [ "to-fix/dependencies/D.ju",
                        "to-fix/dependencies/A.ju",
                        "to-fix/dependencies/B.ju",
                        "to-fix/dependencies/C.ju"
                      ]
              )
          let recd = Traverse.recGroups c
          correctOrder T.@=? fmap (\(x :| []) -> Traverse.name x) recd
