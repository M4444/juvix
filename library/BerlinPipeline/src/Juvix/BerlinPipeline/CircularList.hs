module Juvix.BerlinPipeline.CircularList where

import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
-- import qualified Juvix.BerlinPipeline.RecList as RecList


data T a = T 
  { name :: Maybe NameSymbol.T
  , value :: RecList a 
  }
  deriving (Show, Eq)

instance Semigroup (T a) where
  T _ l <> T n r = T n (r <> l)

data RecList a 
    = Rec [RecList a]
    | Anu a
    deriving (Show, Eq)

instance Semigroup (RecList a) where
  Rec l <> Rec r = Rec $ r <> l
  Rec l <> a@(Anu _) = Rec $ a : l
  a@(Anu _) <> Rec r = Rec .reverse $ a : r
  l@(Anu _) <> r@(Anu _) = Rec $ r : [l]

init :: NameSymbol.T -> T a
init sym = T (Just sym) (Rec [])

initAnu :: NameSymbol.T -> a -> T a
initAnu sym v = T (Just sym) (Anu v)

empty :: T a
empty = T Nothing (Rec [])
