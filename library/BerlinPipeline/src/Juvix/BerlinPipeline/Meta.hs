module Juvix.BerlinPipeline.Meta
  ( Feedback,
    T (..),
    Juvix.BerlinPipeline.Meta.empty,
    HasMeta,
    Juvix.BerlinPipeline.Meta.put,
    Juvix.BerlinPipeline.Meta.get,
  )
where

import qualified Juvix.BerlinPipeline.Feedback as Feedback
import Juvix.Library
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp

data Feedback = Feedback
  deriving (Eq, Show)

data T = T
  { feedback :: Feedback.T,
    trace :: Trace.T
  }
  deriving (Show, Eq)

empty :: T
empty = T Feedback.empty Trace.empty

type HasMeta m =
  (HasThrow "error" Sexp.T m, Trace.Eff m, Feedback.Eff m)

put :: (Trace.Eff m, Feedback.Eff m) => T -> m ()
put T {feedback, trace} = do
  Juvix.Library.put @"feedback" feedback
  Juvix.Library.put @"trace" trace

get :: (Trace.Eff m, Feedback.Eff m) => m T
get =
  T <$> Juvix.Library.get @"feedback" <*> Juvix.Library.get @"trace"
