{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Context.Types where

import Control.Lens hiding ((|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import GHC.Show
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Context.Open as Open
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import qualified StmContainers.Map as STM
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read (readsPrec))
import qualified Prelude (error)

data T = T
  { currentNameSpace :: InfoRecord,
    currentName :: NameSymbol.T,
    topLevelMap :: HashMap.T Symbol Info,
    reverseLookup :: ReverseLookup
  }
  deriving (Show, Read, Eq, Generic, NFData)

data Definition
  = Term Sexp.T
  | Module Module
  | -- | @CurrentNameSpace@ Signifies that this path is the current
    -- module, and that we should search the currentNameSpace instead
    CurrentNameSpace
  deriving (Show, Read, Generic, Eq, NFData)

data Module = Mod
  { moduleContents :: NameSpace.T Info,
    moduleOpenList :: [Open.TName NameSymbol.T],
    moduleIncludeList :: [NameSymbol.T],
    moduleQualifiedMap :: SymbolMap
  }
  deriving (Show, Read, Generic, Eq, NFData)

data Info = Info
  { infoTable :: HashMap.T Symbol Sexp.T,
    infoDef :: Definition
  }
  deriving (Show, Read, Generic, Eq, NFData)

data InfoRecord = InfoRecord
  { infoRecordTable :: HashMap.T Symbol Sexp.T,
    infoRecordRecord :: Module
  }
  deriving (Show, Read, Generic, Eq, NFData)

--------------------------------------------------------------------------------
-- Insertion and Lookup Data Types
--------------------------------------------------------------------------------

-- | From constitutes where the value we are looking up comes from
-- Does it come from the Current name space, or does it come from some
-- name space from the global map
data From = From
  { fromNameSpace :: NameSpace,
    fromQualifedName :: NameSymbol.T,
    fromTerm :: Info
  }
  deriving (Show, Eq)

data NameSpace
  = Public
  | Private
  | Outside
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- info conversion functions
--------------------------------------------------------------------------------

infoToInfoRecordErr :: Info -> InfoRecord
infoToInfoRecordErr Info {infoTable, infoDef} =
  case infoDef of
    Module record ->
      InfoRecord {infoRecordTable = infoTable, infoRecordRecord = record}
    _ ->
      Prelude.error "non record sent into coercsion in infoToInfoRecordErr"

infoRecordToInfo :: InfoRecord -> Info
infoRecordToInfo InfoRecord {infoRecordTable, infoRecordRecord} =
  Info {infoTable = infoRecordTable, infoDef = Module infoRecordRecord}

instance NFData (STM.Map k v) where rnf x = seq x ()
--------------------------------------------------------------------------------
-- Error data types
--------------------------------------------------------------------------------

newtype PathError
  = VariableShared NameSymbol.T
  deriving (Show, Read, Eq, Generic)
  deriving newtype (A.ToJSON, A.FromJSON)

--------------------------------------------------------------------------------
-- Symbol Location Types
--------------------------------------------------------------------------------

data WhoUses = Who
  { impExplict :: Open.T,
    modName :: NameSymbol.T,
    symbolMap :: SymbolMap
  }
  deriving (Show, Read, Eq, Generic, NFData)

type SymbolMap = STM.Map Symbol SymbolInfo

type ReverseLookup = HashMap.T NameSymbol.T [WhoUses]

-- Note ∷ we don't store the implicit explicit open nature of the symbol
-- this can be found by querying the reverse map and seeing there
-- this is sadly O(n) right now... but can be made faster in the future
data SymbolInfo = SymInfo
  { -- | used notes if the symbol is used and if so in what
    used :: UsedIn,
    -- | mod is the module where the symbol is coming from
    mod :: NameSymbol.T
  }
  deriving (Show, Read, Eq, Generic)

data UsedIn = Func [Symbol] | NotUsed | Yes deriving (Show, Read, Eq, Generic)

instance Show (STM.Map a b) where
  show _ = "map"

instance Read (STM.Map a b) where
  -- I'm sorry :(
  readsPrec _ _ = [(unsafePerformIO $ atomically STM.new, "")]

instance Show (STM (STM.Map a b)) where
  show _ = "STM map"

-- for the sake of our types, we are just going to ignore any value in
-- the STM map
instance Eq (STM a) where
  _ == _ = True

instance Eq (STM.Map a b) where
  _ == _ = True

-- not using lenses anymore but leaving this here anyway
makeLensesWith camelCaseFields ''Definition

makeLensesWith camelCaseFields ''Module

makeLensesWith camelCaseFields ''Info

makeLensesWith camelCaseFields ''InfoRecord

makeLensesWith camelCaseFields ''From

-- to avoid refactor we just add _ infront
makeLensesFor
  [ ("currentNameSpace", "_currentNameSpace"),
    ("currentName", "_currentName"),
    ("topLevelMap", "_topLevelMap"),
    ("reverseLookup", "_reverseLookup")
  ]
  ''T

--------------------------------------------------------------------------------
-- Special Names
--------------------------------------------------------------------------------

topLevelName :: IsString p => p
topLevelName = "TopLevel"

--------------------------------------------------------------------------------
-- Aeson instances
--------------------------------------------------------------------------------

instance A.ToJSON T where
  toJSON = defaultTo

instance A.FromJSON T where
  parseJSON = defaultFrom

instance A.ToJSON InfoRecord where
  toJSON = defaultTo

instance A.FromJSON InfoRecord where
  parseJSON = defaultFrom

instance A.ToJSON Info where
  toJSON = defaultTo

instance A.FromJSON Info where
  parseJSON = defaultFrom

instance A.ToJSON Definition where
  toJSON = defaultTo

instance A.FromJSON Definition where
  parseJSON = defaultFrom

instance A.ToJSON Module where
  toJSON = defaultTo

instance A.FromJSON Module where
  parseJSON = defaultFrom

instance A.ToJSON WhoUses where
  toJSON = defaultTo

instance A.FromJSON WhoUses where
  parseJSON = defaultFrom

defaultTo :: (Generic a, A.GToJSON' A.Value A.Zero (Rep a)) => a -> A.Value
defaultTo =
  A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField}
    |> A.genericToJSON

defaultFrom :: (Generic a, A.GFromJSON A.Zero (Rep a)) => A.Value -> A.Parser a
defaultFrom =
  A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField}
    |> A.genericParseJSON

instance A.ToJSON (STM.Map a b) where
  toJSON _ = A.object []

instance A.FromJSON (STM.Map a b) where
  -- I'm sorry, too
  parseJSON = pure $ pure (unsafePerformIO $ atomically STM.new)
