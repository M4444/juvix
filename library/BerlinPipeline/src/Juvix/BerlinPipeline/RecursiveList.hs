-- |
--
-- A Recursive list represents the concept of a list with a chance to
-- nest. This allows us to get a tree like structure, where a pipeline
-- step can contain more steps.
module Juvix.BerlinPipeline.RecursiveList where

import qualified Data.HashSet as Set
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T a
  = Rec [T a] (Maybe NameSymbol.T)
  | Anu a
  deriving (Show, Eq)

instance Semigroup (T a) where
  -- This rule reduces nesting of non named groups
  Rec l Nothing <> right = Rec (l <> [right]) Nothing
  -- this rule is for all other scenarios
  left <> right = Rec [left, right] Nothing

snoc :: a -> T a -> T a
snoc ele (Anu anuuu) = Rec [Anu anuuu, Anu ele] Nothing
snoc ele (Rec rec n) = Rec (rec <> [Anu ele]) n

cons :: a -> T a -> T a
cons ele (Anu anuuu) = Rec [Anu ele, Anu anuuu] Nothing
cons ele (Rec rec n) = Rec (Anu ele : rec) n

removeFirstNested :: T a -> T a
removeFirstNested (Anu _) = Rec [] Nothing
removeFirstNested (Rec [] name) = Rec [] name
removeFirstNested (Rec (x : xs) name) =
  case removeFirstNested x of
    Rec [] _ -> Rec xs name
    otherwise -> Rec (otherwise : xs) name

namesToFirstTerm :: T a -> Set.HashSet NameSymbol.T
namesToFirstTerm (Anu _) =
  Set.empty
namesToFirstTerm (Rec [] name) =
  -- there is no more terms!
  maybe Set.empty Set.singleton name
namesToFirstTerm (Rec (x : _) Nothing) =
  namesToFirstTerm x
namesToFirstTerm (Rec (x : _) (Just name)) =
  Set.insert name (namesToFirstTerm x)

firstNested :: T a -> Maybe a
firstNested (Anu a) = Just a
firstNested (Rec [] _) = Nothing
firstNested (Rec (x : _) _) = firstNested x

nameGroup :: NameSymbol.T -> T a -> T a
nameGroup name (Rec xs Nothing) = Rec xs (Just name)
nameGroup name otherCase = Rec [otherCase] (Just name)

groupOf :: NameSymbol.T -> [T a] -> T a
groupOf name xs = Rec xs (Just name)

empty :: T a
empty = Rec [] Nothing
