{-# LANGUAGE TemplateHaskell #-}

module Juvix.BerlinPipeline.Meta
  ( T (..),
    Juvix.BerlinPipeline.Meta.empty,
    HasMeta,

    -- * API Function
    Juvix.BerlinPipeline.Meta.put,
    Juvix.BerlinPipeline.Meta.get,
    info,

    -- * Lenses
    trace,
    feedback,
    HasTrace,
    HasFeedback,
  )
where

import Control.Lens as Lens hiding ((|>))
import qualified Control.Lens.TH as TH
import qualified Juvix.BerlinPipeline.Feedback as Feedback
import Juvix.Library hiding (trace)
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp

data T = T
  { _feedback :: Feedback.T,
    _trace :: Trace.T
  }
  deriving (Show, Eq)

TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''T

empty :: T
empty = T Feedback.empty Trace.empty

type HasMeta m =
  (HasThrow "error" Sexp.T m, Trace.Eff m, Feedback.Eff m)

put :: (Trace.Eff m, Feedback.Eff m) => T -> m ()
put t = do
  Juvix.Library.put @"feedback" (t ^. feedback)
  Juvix.Library.put @"trace" (t ^. trace)

get :: (Trace.Eff m, Feedback.Eff m) => m T
get =
  T <$> Juvix.Library.get @"feedback" <*> Juvix.Library.get @"trace"

info :: T -> IO ()
info t = do
  Feedback.dumpFeedback (t ^. feedback)
  Trace.info (t ^. trace)
