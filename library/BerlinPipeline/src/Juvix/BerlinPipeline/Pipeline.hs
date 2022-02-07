{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.BerlinPipeline.Pipeline where

import Control.Lens as Lens hiding ((|>))
import qualified Control.Lens.TH as TH
import qualified Juvix.BerlinPipeline.Feedback as Feedback
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.Context as Context
import Juvix.Library hiding (trace)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.Sexp as Sexp

--------------------------------------------------------------------------------
-- Reference Types
--------------------------------------------------------------------------------

-- | @EnvOrSexp@ expresses data that may be already added to the
-- environment or are still in a base sexp form that requires no
-- lookup
data EnvOrSexp
  = InContext NameSymbol.T
  | Sexp Sexp.T
  deriving (Show, Eq, Generic)

-- | @Around@ represents when a pass modifier that runs before each
-- pass runs on a singular @EnvOrSexp@.
data Around
  = Before
  | After
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Input and Environment
--------------------------------------------------------------------------------

-- | @CIn@ is the actual data passed into the @Step.T@ type. The
-- @_languageData@ field serves as the direct data that the direct
-- data a pass wishes to take, while @surroundingData@ serves as extra
-- environment constraints that one wants to include.
data CIn = CIn
  { _languageData :: WorkingEnv,
    _surroundingData :: SurroundingEnv
  }
  deriving (Show, Generic)

-- | @WorkingEnv@ serves as the input data relevant to any pipeline
-- step function. This is what is actively being processed in the
-- pipeline itself.
data WorkingEnv = WorkingEnv
  { _currentExp :: [EnvOrSexp],
    _context :: Context.T
  }
  deriving (Show, Eq, Generic)

-- | @SurroundingEnv@ serves as the minimum surrounding information in
-- the environment of the Pipeline Steps.
data SurroundingEnv = SurroundingEnv
  { -- | The @_currentStepName@ represents the current running step
    -- name
    _currentStepName :: Maybe NameSymbol.T,
    _metaInfo :: Meta.T,
    -- | @_onSinglePass@ denotes the list of functions that ought to
    -- be run before or after every pass on a single @EnvOrSexp@
    _onSinglePass :: [(Around, PassArgument -> AroundMIO PassArgument)]
  }
  deriving (Show, Generic)

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
  deriving (Generic)

class HasExtract a where
  extract :: a x -> IO (COut x)

--------------------------------------------------------------------------------
-- Automation Type Declarations
--------------------------------------------------------------------------------

-- | @Job@ denotes the return type of the Automation module. It tells
-- what changes ought to be done to the Environment.
data Job
  = -- | @ProcessJob@ is a pass that does not touch the context, thus
    -- the context does not change itself.
    ProcessJob ProcessJobNoEnv
  | -- | UpdateJob@ indicates that we have changed the context, so thus
    -- we should commit to back to get a new context.
    UpdateJob
      { newContext :: Context.T Sexp.T Sexp.T Sexp.T,
        process :: ProcessJob
      }
  deriving (Show)

----------------------------------------
-- Input Type
----------------------------------------

-- | @PassArgument@ is a pipeline processing function, namely we wrap
-- the current expression and the context into a single function
data PassArgument = PassArgument
  { _current :: EnvOrSexp,
    _context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

-- | @SimplifiedPassArgument@ represents the simplified version of
-- @PassArgument@ for whose passes simply don't care about where the
-- sexp comes from.
data SimplifiedPassArgument = SimplifiedArgument
  { _current :: Sexp.T,
    _context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

----------------------------------------
-- Output Types
----------------------------------------

-- | @Stage@ gives us the concept of staging of new passes
data Stage
  = -- | Current would act exactly like adding any new expression to
    -- the @_currentExp@ list.
    Current
  | -- | FromTopToCurrent acts almost like Current but runs all passes
    -- that have previous ran to the current position on the new
    -- @Sexp.T@ defined
    FromTopToCurrent
  | -- | @Eval@ runs the entire compiler on the newly defined @Sexp.T@,
    --  updating the Context in the process before we proceed with the
    --  current form.
    Eval
  deriving (Show, Eq)

-- | @ProcessJob@ allows us to write passes which instead of adding it
-- to the @_currentExp@ list, we instead add it to the @_newForms@,
-- letting the Automation handle how this commits back to
-- @_currentExp@. The current expression stays as the current
-- expression, thus the pass takes a @EnvOrSexp@ and gives back a
-- potentially new @Sexp.T@, with any new definitions as @_newForms@.
data ProcessJob = Process
  { _current :: EnvOrSexp,
    _newForms :: [(Stage, EnvOrSexp)]
  }
  deriving (Show, Eq, Generic)

-- | @ProcessJobNoEnv@ is like @ProcessJob@ but it does not contain
-- any reference to names that might be stored in the @Context.T@
data ProcessJobNoEnv = ProcessNoEnv
  { _current :: Sexp.T,
    _newForms :: [(Stage, Sexp.T)]
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- Minimal Environment
--------------------------------------------------------------------------------

-- Have to put it here to get rid of the Generic in CIn and the line

data AroundEnv = AroundEnv
  { trace :: Trace.T,
    feedback :: Feedback.T
  }
  deriving (Generic, Show)

type AroundAliasIO =
  ExceptT Sexp.T (StateT AroundEnv IO)

newtype AroundMIO a = MinIO {_runIO :: AroundAliasIO a}
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasState "feedback" Feedback.T,
      HasSource "feedback" Feedback.T,
      HasSink "feedback" Feedback.T
    )
    via StateField "feedback" AroundAliasIO
  deriving
    ( HasState "trace" Trace.T,
      HasSource "trace" Trace.T,
      HasSink "trace" Trace.T
    )
    via StateField "trace" AroundAliasIO
  deriving
    (HasThrow "error" Sexp.T)
    via MonadError AroundAliasIO

runEnv :: AroundMIO a -> Meta.T -> IO (Either Sexp.T a, AroundEnv)
runEnv (MinIO a) (Meta.T {_trace, _feedback}) =
  runStateT (runExceptT a) AroundEnv {trace = _trace, feedback = _feedback}

extractAroundEnv ::
  AroundMIO PassArgument -> Meta.T -> IO (Either Sexp.T (PassArgument, Meta.T))
extractAroundEnv a env = do
  (either, env) <- runEnv a env
  let meta = Meta.T {_trace = trace env, _feedback = feedback env}
  fmap (\pass -> (pass, meta)) either |> pure

--------------------------------------------------------------------------------
-- Lens
--------------------------------------------------------------------------------

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
