{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.BerlinPipeline.Pipeline where

import Control.Lens as Lens hiding ((|>))
import qualified Control.Lens.TH as TH
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp

--------------------------------------------------------------------------------
-- Reference Types
--------------------------------------------------------------------------------

data EnvOrSexp
  = InContext NameSymbol.T
  | Sexp Sexp.T
  deriving (Show, Eq, Generic)

data Around
  = Before
  | After
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Input and Environment
--------------------------------------------------------------------------------

-- | Computational Input
data CIn = CIn
  { _languageData :: WorkingEnv,
    _surroundingData :: SurroundingEnv
  }
  deriving (Show, Eq, Generic)

data WorkingEnv = WorkingEnv
  { _currentExp :: [EnvOrSexp],
    _context :: Context.T
  }
  deriving (Show, Eq, Generic)

data SurroundingEnv = SurroundingEnv
  { _currentStepName :: Maybe NameSymbol.T,
    _metaInfo :: Meta.T,
    _onSexp :: [(Around, ProcessJob)]
  }
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

-- | Computational Output
data COut a
  = Success
      { _meta :: Meta.T,
        _result :: a
      }
  | Failure
      { _meta :: Meta.T,
        _partialResult :: Maybe a
      }
  deriving (Eq, Generic)

class HasExtract a where
  extract :: a x -> IO (COut x)

--------------------------------------------------------------------------------
-- Automation Type Declarations
--------------------------------------------------------------------------------

data Job
  = ProcessJob ProcessJobNoEnv
  | UpdateJob
      { newContext :: Context.T Sexp.T Sexp.T Sexp.T,
        process :: ProcessJob
      }
  deriving (Show)

----------------------------------------
-- Output Types
----------------------------------------

data Stage
  = Current
  | FromTopToCurrent
  | Eval
  deriving (Show, Eq)

data ProcessJob = Process
  { _current :: EnvOrSexp,
    _newForms :: [(Stage, EnvOrSexp)]
  }
  deriving (Show, Eq, Generic)

data ProcessJobNoEnv = ProcessNoEnv
  { _current :: Sexp.T,
    _newForms :: [(Stage, Sexp.T)]
  }
  deriving (Show)

----------------------------------------
-- Input Type
----------------------------------------

data PassArgument = PassArgument
  { _current :: EnvOrSexp,
    _context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

data SimplifiedPassArgument = SimplifiedArgument
  { _current :: Sexp.T,
    _context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''PassArgument
TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''SimplifiedPassArgument

TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''ProcessJobNoEnv
TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''COut

TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''CIn
TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''WorkingEnv
TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''SurroundingEnv

TH.makeLensesWith TH.classUnderscoreNoPrefixFields ''ProcessJob

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | @emptyInput@ is a Computational Input with the @SurroundingEnv@
-- being blank
emptyInput :: WorkingEnv -> CIn
emptyInput _languageData =
  CIn {_languageData, _surroundingData = startingSurroundingEnv}

startingSurroundingEnv :: SurroundingEnv
startingSurroundingEnv = SurroundingEnv Nothing Meta.empty []

setNameCIn :: NameSymbol.T -> CIn -> CIn
setNameCIn n =
  set (surroundingData . currentStepName) (Just n)

setMetaCIn :: Meta.T -> CIn -> CIn
setMetaCIn meta =
  set (surroundingData . metaInfo) meta

modifyTraceCIn :: (Trace.T -> Trace.T) -> CIn -> CIn
modifyTraceCIn =
  over (surroundingData . metaInfo . Meta.trace)

----------------------------------------
-- Functions on COut
----------------------------------------

getMeta :: COut a -> Meta.T
getMeta cout = cout ^. meta
