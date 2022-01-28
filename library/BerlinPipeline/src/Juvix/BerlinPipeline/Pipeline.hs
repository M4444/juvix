{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.BerlinPipeline.Pipeline where

import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

--------------------------------------------------------------------------------
-- Reference Types
--------------------------------------------------------------------------------

data EnvOrSexp
  = InContext NameSymbol.T
  | Sexp Sexp.T
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Input and Environment
--------------------------------------------------------------------------------

-- | Computational Input
data CIn = CIn
  { languageData :: WorkingEnv,
    surroundingData :: SurroundingEnv
  }
  deriving (Show, Eq, Generic)

data WorkingEnv = WorkingEnv
  { currentExp :: [EnvOrSexp],
    context :: Context.T
  }
  deriving (Show, Eq, Generic)

data SurroundingEnv = SurroundingEnv
  { currentStepName :: Maybe NameSymbol.T,
    metaInfo :: Meta.T
  }
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

-- | Computational Output
data COut a
  = Success
      { meta :: Meta.T,
        result :: a
      }
  | Failure
      { meta :: Meta.T,
        partialResult :: Maybe a
      }
  deriving (Eq, Generic)

class HasExtract a where
  extract :: a x -> IO (COut x)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | @emptyInput@ is a Computational Input with the @SurroundingEnv@
-- being blank
emptyInput :: WorkingEnv -> CIn
emptyInput languageData =
  CIn {languageData, surroundingData = startingSurroundingEnv}

startingSurroundingEnv :: SurroundingEnv
startingSurroundingEnv = SurroundingEnv Nothing Meta.empty

setNameCIn :: NameSymbol.T -> CIn -> CIn
setNameCIn n cIn =
  cIn
    { surroundingData =
        let d = surroundingData cIn in d {currentStepName = Just n}
    }

setMetaCIn :: Meta.T -> CIn -> CIn
setMetaCIn meta cIn =
  cIn
    { surroundingData =
        let d = surroundingData cIn in d {metaInfo = meta}
    }

----------------------------------------
-- Functions on COut
----------------------------------------

getMeta :: COut a -> Meta.T
getMeta Success {meta} = meta
getMeta Failure {meta} = meta
