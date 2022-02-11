{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Juvix.Context.Types where

import Control.Lens hiding ((|>))
import qualified Data.Aeson as A
import GHC.Show
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Context.Open as Open
import Juvix.Context.Precedence
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified StmContainers.Map as STM
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read (readsPrec))

data T term ty sumRep = T
  { currentNameSpace :: Record term ty sumRep,
    currentName :: NameSymbol.T,
    topLevelMap :: HashMap.T Symbol (Definition term ty sumRep),
    reverseLookup :: ReverseLookup
  }
  deriving (Show, Read, Eq, Generic, NFData)

type NameSpace term ty sumRep = NameSpace.T (Definition term ty sumRep)

-- | From constitutes where the value we are looking up comes from
-- Does it come from the Current name space, or does it come from some
-- name space from the global map
data From b
  = Current (NameSpace.From b)
  | Outside b
  deriving (Show, Functor, Traversable, Foldable, Eq)

-- TODO :: make known records that are already turned into core
-- this will just emit the proper names we need, not any terms to translate
-- once we hit core, we can then populate it with the actual forms
data Definition term ty sumRep
  = Def (Def term ty)
  | Record (Record term ty sumRep)
  | TypeDeclar
      { definitionRepr :: sumRep
      }
  | Unknown
      { definitionMTy :: Maybe ty
      }
  | Information
      { definitionInfo :: [Information]
      }
  | -- Signifies that this path is the current module, and that
    -- we should search the currentNameSpace from here
    CurrentNameSpace
  | SumCon (SumT term ty)
  deriving (Show, Read, Generic, Eq, NFData)

data Def term ty = D
  { defUsage :: Maybe Usage.T,
    defMTy :: Maybe ty,
    defTerm :: term,
    defPrecedence :: Precedence
  }
  deriving (Show, Read, Generic, Eq, Data, NFData)

data SumT term ty = Sum
  { sumTDef :: Maybe (Def term ty),
    sumTName :: Symbol
  }
  deriving (Show, Read, Generic, Eq, Data, NFData)

data Record term ty sumRep = Rec
  { recordContents :: NameSpace.T (Definition term ty sumRep),
    -- Maybe as I'm not sure what to put here for now
    -- TODO ∷ reconsider the type when we have proper module typing up.
    recordMTy :: Maybe ty,
    recordOpenList :: [Open.TName NameSymbol.T],
    recordQualifiedMap :: SymbolMap
  }
  deriving (Show, Read, Generic, Eq, NFData)

instance NFData (STM.Map k v) where rnf x = seq x ()

newtype Information
  = Prec Precedence
  deriving (Show, Read, Generic, Eq, Data, NFData)
  deriving newtype (A.ToJSON, A.FromJSON)

instance Hashable Information where
  hash (Prec pred) = hash pred

newtype PathError
  = VariableShared NameSymbol.T
  deriving (Show, Read, Eq, Generic)
  deriving newtype (A.ToJSON, A.FromJSON)

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

makeLensesWith camelCaseFields ''Def

makeLensesWith camelCaseFields ''Record

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

instance
  (A.ToJSON term, A.ToJSON ty, A.ToJSON sumRep) =>
  A.ToJSON (T term ty sumRep)
  where
  toJSON = defaultTo

instance
  (A.FromJSON term, A.FromJSON ty, A.FromJSON sumRep) =>
  A.FromJSON (T term ty sumRep)
  where
  parseJSON = defaultFrom

instance
  (A.ToJSON term, A.ToJSON ty, A.ToJSON sumRep) =>
  A.ToJSON (Definition term ty sumRep)
  where
  toJSON = defaultTo

instance
  (A.FromJSON term, A.FromJSON ty, A.FromJSON sumRep) =>
  A.FromJSON (Definition term ty sumRep)
  where
  parseJSON = defaultFrom

instance (A.ToJSON term, A.ToJSON ty) => A.ToJSON (Def term ty) where
  toJSON = defaultTo

instance (A.FromJSON term, A.FromJSON ty) => A.FromJSON (Def term ty) where
  parseJSON = defaultFrom

instance (A.ToJSON term, A.ToJSON ty) => A.ToJSON (SumT term ty) where
  toJSON = defaultTo

instance (A.FromJSON term, A.FromJSON ty) => A.FromJSON (SumT term ty) where
  parseJSON = defaultFrom

instance
  (A.ToJSON term, A.ToJSON ty, A.ToJSON sumRep) =>
  A.ToJSON (Record term ty sumRep)
  where
  toJSON = defaultTo

instance
  (A.FromJSON term, A.FromJSON ty, A.FromJSON sumRep) =>
  A.FromJSON (Record term ty sumRep)
  where
  parseJSON = defaultFrom

instance A.ToJSON WhoUses where
  toJSON = defaultTo

instance A.FromJSON WhoUses where
  parseJSON = defaultFrom

defaultTo :: (Generic a, A.GToJSON' A.Value A.Zero (Rep a)) => a -> A.Value
defaultTo =
  A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField}
    |> A.genericToJSON

defaultFrom =
  A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField}
    |> A.genericParseJSON

instance A.ToJSON (STM.Map a b) where
  toJSON _ = A.object []

instance A.FromJSON (STM.Map a b) where
  -- I'm sorry, too
  parseJSON = pure $ pure (unsafePerformIO $ atomically STM.new)
