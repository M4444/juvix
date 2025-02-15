{-# LANGUAGE DeriveGeneric #-}

-- | Closure.T serves as the data structure in which we will store
-- temporary lexical bindings as our code encounters binders.
module Juvix.Closure
  ( Information (..),
    T (..),
    Info (..),
    insert,
    insertGeneric,
    keys,
    lookup,
    precedenceof,
    empty,

    -- * Extended API
  )
where

import qualified Data.HashSet as Set
import Juvix.Context.Precedence
import Juvix.Library hiding (empty)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

-- Currently we don't really use the signature however in the future
-- the mSig will be used to detect the types of modules we will have
-- open and any other information we wish to track here!?
data Information = Info
  { -- | @mSig@ represents the type of the term in the closure
    mSig :: Maybe Sexp.T,
    -- | @info@ represents all the information we have on the term
    info :: [Info],
    -- | @mOpen@ represents a place where the term may have come
    -- from
    mOpen :: Maybe NameSymbol.T
  }
  deriving (Show, Eq, Generic)

newtype Info
  = Prec Precedence
  deriving (Show, Read, Generic, Eq, Data, NFData)

instance Hashable Info where
  hash (Prec pred) = hash pred

newtype T
  = T (Map.T Symbol Information)
  deriving (Show, Eq, Generic)

instance Hashable Information where
  hash (Info {mSig, info, mOpen}) = hash (hash mSig, hash info, hash mOpen)

instance Hashable T where
  hash (T m) = hash m

insert :: Symbol -> Information -> T -> T
insert k info (T m) =
  T $ Map.insert k info m

insertGeneric :: Symbol -> T -> T
insertGeneric name (T m) =
  T $ Map.insert name (Info Nothing [] Nothing) m

keys :: T -> Set.HashSet Symbol
keys (T m) = Map.keysSet m

lookup :: Symbol -> T -> Maybe Information
lookup k (T m) = Map.lookup k m

precedenceof :: Information -> Maybe Precedence
precedenceof (Info {info}) =
  find (\case Prec _ -> True) info >>| \(Prec a) -> a

empty :: T
empty = T Map.empty

insertHash :: Information -> T -> (Symbol, T)
insertHash info t =
  let name = intern . show $ hash info
   in (name, insert name info t)

merge :: T -> T -> T
merge (T m) (T m') = T $ Map.union m m'
