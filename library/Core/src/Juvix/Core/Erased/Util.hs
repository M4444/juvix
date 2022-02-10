module Juvix.Core.Erased.Util where

import qualified Data.Set as Set
import Juvix.Core.Erased.Types
import Juvix.Library hiding (Type)
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | Retrieve the list of variable names that are not bound,
-- i.e. those that are free
free :: Term primVal -> [NameSymbol.T]
free = Set.toList . go Set.empty
  where
    go bound = \case
      Var s -> if Set.member s bound then Set.empty else Set.singleton s
      Prim _ -> Set.empty
      Lam v b -> go (Set.insert v bound) b
      Pair s t -> go bound s <> go bound t
      CatProductIntro s t -> go bound s <> go bound t
      CatProductElimLeft a s -> go bound a <> go bound s
      CatProductElimRight a s -> go bound a <> go bound s
      CatCoproductIntroLeft s -> go bound s
      CatCoproductIntroRight s -> go bound s
      CatCoproductElim a b cp s t -> Set.unions
        [go bound a, go bound b, go bound cp, go bound s, go bound t]
      Unit -> Set.empty
      Record flds -> foldMap (foldMap $ go bound) flds
      RecElim ns e t -> go bound e <> go boundNs t
        where boundNs = foldr Set.insert bound ns
      Let v b t -> go bound b <> go (Set.insert v bound) t
      App a b -> go bound a <> go bound b
