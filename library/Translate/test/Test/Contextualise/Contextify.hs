{-# LANGUAGE LiberalTypeSynonyms #-}

module Test.Contextualise.Contextify where

import Control.Lens ((^.))
import qualified Juvix.Context as Context
import qualified Juvix.Context.InfoNames as Info
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Desugar as DesugarS
import Juvix.Library
import qualified Juvix.Library.Parser.Internal as Internal
import qualified Juvix.Parsing.Parser as Parser
import qualified Juvix.Parsing.Types as AST
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Transition as Structure
import qualified Juvix.Translate.Pipeline.TopLevel as TopLevel
import Test.Context.Helpers (desugarLisp, emptyContextify)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Top
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "contextify tests:"
    [sexpression]

sexpression :: T.TestTree
sexpression =
  T.testGroup
    "s-expression contextify tests:"
    [sumConTestS, defunTransfomrationWorks]

--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------

sumConTestS :: T.TestTree
sumConTestS =
  T.testGroup
    "Sum Constructors are properly added:"
    [ T.testCase "Bool properly adds True" (test "True"),
      T.testCase "Bool properly adds False" (test "False")
    ]
  where
    test str = do
      xs <- desugared
      Right (ctx, _) <-
        emptyContextify (("Foo", xs) :| [])
      ctx Context.!? str
        |> fmap ((^. Context.def) . Context.extractValue)
        |> (T.@=? Just (Context.Term (Sexp.serialize (Structure.SumCon "bool"))))
    Right desugared =
      extract "type bool = True | False"

defunTransfomrationWorks :: T.TestTree
defunTransfomrationWorks =
  T.testCase "defun properly added" test
  where
    test = do
      xs <- desugared
      Right (ctx, _) <-
        emptyContextify (("Foo", xs) :| [])
      let Just info =
            ctx Context.!? "foo" >>| Context.extractValue
          Context.Term x =
            info ^. Context.def
      [Context.lookupInfoSexp info Info.signature, Just x]
        T.@=? [Just sig, Just function]
    Right desugared =
      extract "sig foo : int -> int let foo 1 = 1 let foo n = n * foo (pred n)"
    Right function =
      Sexp.parse "(:lambda-case ((1) 1) ((n) (:infix * n (foo (:paren (pred n))))))"
    Right sig =
      Sexp.parse "(:infix -> int int)"

extract :: ByteString -> Either Internal.ParserError (IO [Sexp.T])
extract s =
  Parser.parse s
    >>| desugarLisp . fmap TopLevel.transTopLevel . AST.extractTopLevel

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------
