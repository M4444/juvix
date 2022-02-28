{-# LANGUAGE DuplicateRecordFields #-}

-- | The Step Module serves as the base interface for any compiler
-- step. The module is quite minimal, with only a few types along with
-- a small set of operations.
--
-- This main type of the module, @T@, is simply a function type that
-- takes a structure type that has more than it needs to do any pass
-- called @Pipeline.COut@ and gives back the result of the computation
-- plus Meta-data.
--
-- We say that the pass takes more than it needs because not only is
-- @Piepline.COut@ composed of the @Pipeline._context@ and the
-- @Pipeline._currentExp@ we are trying to compile against, but also
-- the @Pipeline.SurroundingEnv@ which contains information such as
-- metadata and enough data to fill the environment of any particular
-- pass.
module Juvix.BerlinPipeline.Step
  ( T (..),
    Named (..),
    -- StepMeta(..),
    namePass,
    call,
  )
where

import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | the pipeline step, takes the entire @Pipeline.CIn@ as input.
-- This is quite a wide type in that it has data that a pass does not
-- directly want to handle.
-- Thus it is expected one works with the @Automation@ module, to make
-- fulfilling both the @Pipeline.COut@ and the input form bearable.
data T = T (Pipeline.CIn -> IO (Pipeline.COut Pipeline.WorkingEnv))

call :: T -> Pipeline.CIn -> IO (Pipeline.COut Pipeline.WorkingEnv)
call (T step) = step

data Named = Named
  { name :: NameSymbol.T,
    step :: T
  }

namePass :: NameSymbol.T -> T -> Named
namePass name func = Named name func
