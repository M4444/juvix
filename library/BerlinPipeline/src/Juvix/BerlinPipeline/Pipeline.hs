{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.BerlinPipeline.Pipeline where

import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

data EnvOrSexp
  = InContext NameSymbol.T
  | SExp Sexp.T
  deriving (Show, Eq, Generic)

data WorkingEnv = WorkingEnv
  { currentExp :: [EnvOrSexp],
    context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show, Eq, Generic)

-- | Computational Input
data CIn = CIn
  { languageData :: WorkingEnv
  , surroundingData :: SurroundingEnv
  }
  deriving (Show, Eq, Generic)

data SurroundingEnv = SurroundingEnv
  { currentStepName :: Maybe NameSymbol.T,
    metaInfo :: Meta.T
  }
  deriving (Show, Eq, Generic)

data Success a = Success
  { meta :: Meta.T,
    result :: a
  }
  deriving (Eq, Functor)

data Failure a = Failure
  { meta :: Meta.T,
    partialResult :: Maybe a
  }
  deriving (Eq, Functor)

-- | Computational Output
data COut a
  = COutSuccess (Success a)
  | COutFailure (Failure a)
  deriving (Eq, Generic, Functor, Applicative, Monad)
