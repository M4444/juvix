{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.BerlinPipeline.Step
  ( T (..),
    Named (..),
    -- StepMeta(..),
    register,
    call,
  )
where

import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T = T (Pipeline.CIn -> IO (Pipeline.COut Pipeline.WorkingEnv))

call :: T -> Pipeline.CIn -> IO (Pipeline.COut Pipeline.WorkingEnv)
call (T step) = step

data Named = Named
  { name :: NameSymbol.T,
    step :: T
  }

register :: NameSymbol.T -> T -> Named
register name func = Named name func
