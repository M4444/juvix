{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Library.LineNum where

import Control.Lens
import qualified Data.Aeson as A
import Juvix.Library
  ( Data,
    Eq,
    Generic,
    Hashable (hash),
    Int,
    NFData,
    Ord,
    Read,
    Show,
    Typeable,
  )

data T = T {tLine :: Int, tCol :: Int}
  deriving
    ( Show,
      Eq,
      Ord,
      Data,
      Generic,
      NFData,
      Read,
      Typeable
    )

instance A.ToJSON T where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON T where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance Hashable T where
  hash T {tLine, tCol} = hash (hash tLine, hash tCol)

makeLensesWith camelCaseFields ''T
