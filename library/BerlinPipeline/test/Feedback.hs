module Feedback where

import Juvix.BerlinPipeline.Feedback as Feedback
import Juvix.Library
import qualified Juvix.Sexp as Sexp
import qualified Test.Tasty as T
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top = T.testGroup "feedback tests" [messageTest]

messageResolution =
  Feedback.addMessageNoEff Feedback.Warning (Sexp.string "Not in scope") Feedback.empty
    |> Feedback.addMessageNoEff
      Feedback.Error
      (Sexp.list [Sexp.atom ":resolution-error", Sexp.string "Foo not in scope"])
    |> Feedback.messages
    |> Feedback.formatFeedback

messageTest :: T.TestTree
messageTest =
  T.testCase "two messages in right order" $
    "0 WARN: \"Not in scope\"\n1 ERROR: (\":resolution-error\" \"Foo not in scope\")"
      T.@?= messageResolution
