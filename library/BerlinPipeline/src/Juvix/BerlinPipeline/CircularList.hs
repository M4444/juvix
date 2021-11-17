module Juvix.BerlinPipeline.CircularList where

import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
-- import qualified Juvix.BerlinPipeline.RecList as RecList


data T a = T  (RecList (CircSchema a) )
  deriving (Show, Eq)

instance Semigroup (T a) where
  T l <> T r = T (l <> r)

data CircSchema a 
  = CircSchema [a]
  | NonCircSchema a
  deriving (Show, Eq)


-- Rec [Anu (NonCircSchema Parsing), Anu (NonCircSchema condToIf)]
-- Rec [Anu (CircSchema [Parsing, CondToIf]), Anu (NonCircSchema condToIf)]
data RecList a 
    = Rec [RecList a]
    | Anu a
    deriving (Show, Eq)

firstNested :: T a -> Maybe (CircSchema a) 
firstNested (T (Anu a)) = Just a
firstNested (T (Rec [])) = Nothing
firstNested (T (Rec (x:_xs))) = firstNested (T x)

removeFirstNested :: T a -> T a
removeFirstNested (T (Anu a)) = T (Rec [])
removeFirstNested (T (Rec [])) = T (Rec [])
removeFirstNested (T (Rec (x:xs))) = 
  case removeFirstNested (T x) of
    T (Rec []) -> T (Rec xs)
    T otherwise -> T (Rec (otherwise : xs))

instance Semigroup (RecList a) where
  Rec l <> Rec r = Rec $ l <> r
  Rec l <> a@(Anu _) = Rec $ l ++ [a]
  a@(Anu _) <> Rec r = Rec $ a : r
  l@(Anu _) <> r@(Anu _) = Rec $ l : [r]

init :: NameSymbol.T -> T a
init sym = T (Rec [])

initAnu :: a -> T a
initAnu v = T (Anu (NonCircSchema v))

empty :: T a
empty = T (Rec [])
