module Juvix.BerlinPipeline.Meta
  ( Feedback,
    Trace,
    T,
    Juvix.BerlinPipeline.Meta.empty,
    HasMeta,
    Juvix.BerlinPipeline.Meta.put,
  )
where

import Juvix.Library
import qualified Juvix.Library.Trace as Trace

data Feedback = Feedback
  deriving (Eq, Show)

data Trace = Trace
  deriving (Eq, Show)

data T = Meta
  { feedback :: Feedback,
    trace :: Trace.T
  }
  deriving (Show, Eq)

empty :: T
empty = Meta Feedback (Trace.empty)

type HasMeta m = (HasState "meta" T m, HasThrow "error" Text m)

put :: HasMeta m => T -> m ()
put = Juvix.Library.put @"meta"
