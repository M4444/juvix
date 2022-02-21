module Test.Sexp.Helpers where

import Control.Lens (view, (^.))
import qualified Juvix.BerlinPasses as BerlinPasses
import qualified Juvix.BerlinPipeline.Env as Pipeline.Env
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Desugar as Desugar
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Parsing.Parser as Parser
import qualified Juvix.Parsing.Types.Base as Parsing
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Translate.Pipeline.TopLevel as TopLevel
import Test.Context.Helpers (runPipelineToStep, runSexpPipelineEnv)
import Prelude (error)

----------------------------------------------------------------------
-- Give me sexp terms helpers
----------------------------------------------------------------------

unwrapLookup :: NameSymbol.T -> Context.T -> Maybe Sexp.T
unwrapLookup symbol ctx =
  case Context.lookup symbol ctx >>| Context.extractValue of
    Just ((Context.Info _ (Context.Term defTerm))) ->
      Just defTerm
    _ -> Nothing

contextualizeFoo ::
  ByteString -> IO Context.T
contextualizeFoo byte = do
  contextualizeFooEnv byte
    >>| view (Pipeline.languageData . Pipeline.context)

contextualizeFooEnv ::
  ByteString -> IO Pipeline.CIn
contextualizeFooEnv byte =
  [("A", juvix), ("Foo", parsedSexp byte)]
    |> runSexpPipelineEnv BerlinPasses.eval
  where
    juvix =
      parsedSexp
        "declare infixl (+) 8 \
        \ let (+) = 3 \
        \ declare infixl (*) 9 \
        \ let (*) = 3 \
        \ declare infix (**) 7 \
        \ let (**) = 3 \
        \ declare infix (***) 7 \
        \ let (**) = 3 \
        \ let a = 3 \
        \ let x = 2 "

contextualizeInclude ::
  IO (Either Contextify.ResolveErr Context.T)
contextualizeInclude =
  Contextify.op
    ( ( "A",
        parseDesugarSexp
          "let x = 2 "
      )
        :| [ ( "Foo",
               parseDesugarSexp
                 "include TopLevel.A"
             ),
             ( "Bar",
               parseDesugarSexp
                 "include TopLevel.Foo \
                 \ let fi = x"
             )
           ]
    )

contextualizeFooAmbi ::
  ByteString -> IO Context.T
contextualizeFooAmbi byte =
  contextualizeFooAmbiEnv byte
    >>| view (Pipeline.languageData . Pipeline.context)

contextualizeFooAmbiEnv ::
  ByteString -> IO Pipeline.CIn
contextualizeFooAmbiEnv byte =
  [ ( "A",
      parsedSexp "declare infixl (+) 9 let (+) = 3 "
    ),
    ("B", parsedSexp "declare infixl (+) 9 let (+) = 3"),
    ("Foo", parsedSexp byte)
  ]
    |> runSexpPipelineEnv BerlinPasses.eval

parseDesugarSexp :: ByteString -> IO [Sexp.T]
parseDesugarSexp = desugarLisp . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| TopLevel.transTopLevel

ignoreHeader :: Either a (Parsing.Header topLevel) -> [topLevel]
ignoreHeader (Right (Parsing.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"

desugarLisp :: [Sexp.T] -> IO [Sexp.T]
desugarLisp xs =
  runPipelineToStep "Context.initContext" [("Juvix-User", xs)]
    >>| view Pipeline.currentExp
    >>| sexps
  where
    sexps xs = [s | (Pipeline.Sexp s) <- xs]
