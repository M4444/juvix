{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.BerlinPipeline.Step
  ( T (..),
    Named (..),
    -- StepMeta(..),
    register,
  )
where

import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T = T (Pipeline.CIn -> IO (Pipeline.COut Pipeline.WorkingEnv))

data Named = Named
  { name :: NameSymbol.T,
    step :: T
  }

-- stepR ::

-- data StepMeta = StepMeta
--   { name :: Maybe NameSymbol.T,
--     meta :: Meta.T
--   }

register :: NameSymbol.T -> T -> Named
register name func = Named name func

