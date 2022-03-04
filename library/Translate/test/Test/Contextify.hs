module Test.Contextify (top) where

import Control.Lens as Lens hiding ((|>))
import qualified Juvix.BerlinPipeline.Feedback as Feedback
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.Environment as Env
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as Contextify
import Juvix.Library
import qualified Juvix.Sexp as Sexp
import Test.Sexp.Helpers
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "testing desugaring passes functions"
    [moduleResolution, infixResolution, lookupResolution]

moduleResolution :: T.TestTree
moduleResolution =
  T.testGroup
    "Testing module resolution and binders"
    [ T.testCase "ignore binder on function arguments" $ do
        t <- contextualizeFoo "open A let fi a = a"
        let Right expected = Sexp.parse "(:lambda-case ((a) a))"
        Just expected T.@=? unwrapLookup "fi" t,
      --
      T.testCase "opening actually resolves names properly" $ do
        t <- contextualizeFoo "open A let fi x = a"
        let Right expected = Sexp.parse "(:lambda-case ((x) TopLevel.A.a))"
        Just expected T.@=? unwrapLookup "fi" t,
      --
      T.testCase "λ properly updates closure" $ do
        t <- contextualizeFoo "open A let fi = \\a -> a"
        let Right expected = Sexp.parse "(:lambda-case (() (:lambda (a) a)))"
        Just expected T.@=? unwrapLookup "fi" t,
      --
      T.testCase "match properly udpates closure" $ do
        t <-
          contextualizeFoo
            "open A let fi = case a of | Cons a (Cons x y) -> add a x y"
        let Right expected =
              Sexp.parse
                "(:lambda-case \
                \  (() (case TopLevel.A.a ((Cons a (Cons x y)) (add a x y)))))"
        Just expected T.@=? unwrapLookup "fi" t,
      T.testCase "infix declaration works with declaration" $ do
        t <-
          contextualizeFoo
            "type foo = (::) declare infixl (::) 20 let fi = a * 3 :: 10"
        let Right expected =
              Sexp.parse
                "(:lambda-case (() (* a (:: 3 10))))"
        Just expected T.@=? unwrapLookup "fi" t,
      T.testCase "infix declaration works with declaration" $ do
        t <-
          contextualizeFoo
            "type foo = (::) declare infixl (::) 1 let fi = a * 3 :: 10"
        let Right expected =
              Sexp.parse "(:lambda-case (() (:: (* a 3) 10)))"
        Just expected T.@=? unwrapLookup "fi" t,
      --
      T.testCase "defining an imported function just shadows" $ do
        t <- contextualizeFoo "open A let fi = x let x = 2"
        let Right expected = Sexp.parse "(:lambda-case (() x))"
        unwrapLookup "fi" t T.@=? Just expected,
      T.testCase "ambiguous imports error" $ do
        t <- contextualizeFooAmbiEnv "open A open B let fi = 2"
        case extractErr t of
          Just (Contextify.Resolve (Contextify.AmbiguousSymbol "+")) -> pure ()
          _ -> T.assertFailure "Expected a Resolve error"
    ]

infixResolution :: T.TestTree
infixResolution =
  T.testGroup
    "Testing infix resolution and clashses"
    [ T.testCase "zzzztwo forms with infix will error" $ do
        t <- contextualizeFooEnv "open A let fi a = a ** a *** a"
        case extractErr t of
          Just (Env.Clash _ _) -> pure ()
          _ -> T.assertFailure "Expected error to be Env.ErrorS.Clash",
      T.testCase "two forms with infix will succeed" $ do
        t <- contextualizeFoo "open A let fi a = 1 * 2 + 3"
        let Right expected = Sexp.parse "(:lambda-case ((a) (TopLevel.A.+ (TopLevel.A.* 1 2) 3)))"
        Just expected T.@=? unwrapLookup "fi" t,
      T.testCase "infix declaration works with declaration" $ do
        t <-
          contextualizeFoo
            "type foo = (::) declare infixl (::) 20 let fi = a * 3 :: 10"
        let Right expected =
              Sexp.parse
                "(:lambda-case (() (* a (:: 3 10))))"
        Just expected T.@=? unwrapLookup "fi" t,
      T.testCase "infix declaration works with declaration" $ do
        t <-
          contextualizeFoo
            "type foo = (::) declare infixl (::) 1 let fi = a * 3 :: 10"
        let Right expected =
              Sexp.parse "(:lambda-case (() (:: (* a 3) 10)))"
        Just expected T.@=? unwrapLookup "fi" t,
      --
      T.testCase "infix declaration works with declaration in match" $ do
        t <-
          contextualizeFoo
            "open A declare infixl (::) 6 let fi (a :: b) = case foo of | a :: b * c -> c"
        let Right expected =
              Sexp.parse "(:lambda-case (((:: a b)) (case foo ((:: a (* b c)) c))))"
        Just expected T.@=? unwrapLookup "fi" t,
      T.testCase "infix declaration works with declaration in match" $ do
        t <-
          contextualizeFoo
            "open A let fi (a :: b) = case foo of | a :: b * c -> c"
        let Right expected =
              Sexp.parse "(:lambda-case (((:: a b)) (case foo ((* (:: a b) c) c))))"
        Just expected T.@=? unwrapLookup "fi" t
    ]

lookupResolution :: T.TestTree
lookupResolution =
  T.testGroup
    "Testing Lookup resolution events"
    [ T.testCase "looking up a field works as expected" $ do
        t <- contextualizeFoo "let bar = let y = 3 in { zzzz = 3 } let foo = bar.zzzz"
        let Right expected = Sexp.parse "(:lambda-case (() (:lookup bar zzzz)))"
        Just expected T.@=? unwrapLookup "foo" t,
      T.testCase "Nothing happens on field lookup on an unbound field" $ do
        t <- contextualizeFoo "let foo = bar.zzzz"
        let Right expected = Sexp.parse "(:lambda-case (() bar.zzzz))"
        Just expected T.@=? unwrapLookup "foo" t,
      T.testCase "looking up multiple fields works" $ do
        t <- contextualizeFoo "let bar = let y = 3 in { zzzz = 3 } let foo = bar.zzzz.a"
        let Right expected = Sexp.parse "(:lambda-case (() (:lookup bar zzzz a)))"
        Just expected T.@=? unwrapLookup "foo" t,
      T.testCase "records don't have constructors added as sum con" $ do
        t <- contextualizeFoo "type foo = {x : int, y : int}"
        Nothing T.@=? unwrapLookup ":record-d" t,
      T.testCase "lookup of Includes works!" $ do
        Right ctx <- contextualizeInclude
        True T.@=? isJust (ctx Context.!? "Bar.x")
        True T.@=? isJust (ctx Context.!? "Foo.x")
    ]

extractErr cin =
  cin
    ^. Pipeline.surroundingData
      . Pipeline.metaInfo
      . Meta.feedback
    |> Feedback.getErrors
    |> head
    >>= Sexp.deserialize . Feedback.contents
