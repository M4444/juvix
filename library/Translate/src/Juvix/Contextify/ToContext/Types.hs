{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Contextify.ToContext.Types where

import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data PassSexp = PS
  { ctxS :: Context.T,
    opensS :: [NameSymbol.T],
    modsDefinedS :: [NameSymbol.T]
  }
  deriving (Show)
