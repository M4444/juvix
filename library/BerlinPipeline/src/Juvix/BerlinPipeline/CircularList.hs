module Juvix.BerlinPipeline.CircularList where

import qualified Data.HashSet as Set
import qualified Juvix.BerlinPipeline.RecursiveList as RecList
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T a = T (RecList.T (CircSchema a))
  deriving (Show, Eq)

instance Semigroup (T a) where
  T l <> T r = T (l <> r)

data CircSchema a
  = CircSchema [a]
  | NonCircSchema a
  deriving (Show, Eq)

firstNested :: T a -> Maybe (CircSchema a)
firstNested (T term) = RecList.firstNested term

removeFirstNested :: T a -> T a
removeFirstNested (T l) = T $ RecList.removeFirstNested l

getRecursiveList :: T a -> RecList.T (CircSchema a)
getRecursiveList (T term) = term

groupOf :: NameSymbol.T -> [T a] -> T a
groupOf name = T . RecList.groupOf name . fmap getRecursiveList

namesToFirstTerm :: T a -> Set.HashSet NameSymbol.T
namesToFirstTerm = RecList.namesToFirstTerm . getRecursiveList

init :: a -> T a
init v = T (RecList.Anu (NonCircSchema v))

empty :: T a
empty = T RecList.empty
