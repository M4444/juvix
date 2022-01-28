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

data Feedback = Feedback
  deriving (Eq, Show)

data Trace = Trace
  deriving (Eq, Show)

data T = Meta
  { feedback :: Feedback,
    trace :: Trace
  }
  deriving (Eq, Show)

empty :: T
empty = Meta Feedback Trace

type HasMeta m = (HasState "meta" T m, HasThrow "error" Text m)

put :: HasMeta m => T -> m ()
put = Juvix.Library.put @"meta"
