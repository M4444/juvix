module Test.Sexp.Helpers where

import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Desugar as Desugar
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Parsing.Parser as Parser
import qualified Juvix.Parsing.Types.Base as Parsing
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Translate.Pipeline.TopLevel as TopLevel
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
  ByteString -> IO (Either Contextify.ResolveErr Context.T)
contextualizeFoo byte =
  Contextify.op
    ( ( "A",
        parseDesugarSexp
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
      )
        :| [("Foo", parseDesugarSexp byte)]
    )

contextualizeFooAmbi ::
  ByteString -> IO (Either Contextify.ResolveErr Context.T)
contextualizeFooAmbi byte =
  Contextify.op
    ( ( "A",
        parseDesugarSexp "declare infixl (+) 9 let (+) = 3 "
      )
        :| [ ("B", parseDesugarSexp "declare infixl (+) 9 let (+) = 3"),
             ("Foo", parseDesugarSexp byte)
           ]
    )

parseDesugarSexp :: ByteString -> [Sexp.T]
parseDesugarSexp = Desugar.op . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| TopLevel.transTopLevel

ignoreHeader :: Either a (Parsing.Header topLevel) -> [topLevel]
ignoreHeader (Right (Parsing.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"
