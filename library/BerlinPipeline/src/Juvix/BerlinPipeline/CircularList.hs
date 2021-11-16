{-# DeriveAnyClass #-}
module Juvix.BerlinPipeline.CircularList where

import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.BerlinPipeline.RecursiveList as RecursiveList


data T a = T 
  { name :: Maybe NameSymbol.T
  , value :: RecursiveList.T (RecursiveSchema a)
  }
  deriving (Show, Eq)

instance Semigroup (T a) where
  T _ (RecursiveList.T l) <> T n (RecursiveList.T r) = T n (RecursiveList.T $ r <> l)

data RecursiveSchema a 
    = Recursive [a]
    | NonRecursive a
    deriving (Show, Eq)

instance Semigroup (RecursiveSchema a) where
  Recursive l <> Recursive r = Recursive $ r <> l
  Recursive l <> NonRecursive r = Recursive $ r : l
  NonRecursive l <> Recursive r = Recursive .reverse $ l : r
  NonRecursive l <> NonRecursive r = Recursive $ r : [l]

init :: NameSymbol.T -> T a
init sym = T (Just sym) (RecursiveList.T $ Recursive [])

initRecursive :: NameSymbol.T -> [a] -> T a
initRecursive sym l = T (Just sym) (RecursiveList.T $ Recursive l)

initNonRecursive :: NameSymbol.T -> a -> T a
initNonRecursive sym v = T (Just sym) (RecursiveList.T $ NonRecursive v)

empty :: T a
empty = T Nothing (RecursiveList.T $ Recursive [])
