module Juvix.BerlinPipeline.Meta
  ( Feedback,
    Trace,
    T,
    Juvix.BerlinPipeline.Meta.empty,
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
