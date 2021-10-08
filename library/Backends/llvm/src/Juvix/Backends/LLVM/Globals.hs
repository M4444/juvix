module Juvix.Backends.LLVM.Globals where

import qualified Data.String as S (fromString)
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | Map of globals along with their names.
type GlobalMap primTy primVal =
  Map.Map NameSymbol.T (ErasedAnn.AnnTerm primTy primVal)

-- | State as used for the globals.
data Globals primTy primVal = Globals
  { globals :: GlobalMap primTy primVal,
    nameGen :: Natural
  }
  deriving (Generic)

-- | New datatype for overloading the capabilities of the global state.
newtype GlobalState primTy primVal a
  = GlobalState (State (Globals primTy primVal) a)
  deriving newtype (Functor, Monad, Applicative)
  deriving
    ( HasState "globals" (GlobalMap primTy primVal),
      HasSink "globals" (GlobalMap primTy primVal),
      HasSource "globals" (GlobalMap primTy primVal)
    )
    via StateField "globals" (State (Globals primTy primVal))
  deriving
    ( HasState "nameGen" Natural,
      HasSink "nameGen" Natural,
      HasSource "nameGen" Natural
    )
    via StateField "nameGen" (State (Globals primTy primVal))

-- | Run the state with a given initial map of globals.
runGlobalState ::
  GlobalState primTy primVal a ->
  Globals primTy primVal ->
  (a, Globals primTy primVal)
runGlobalState (GlobalState stateM) i = runState stateM i

-- | Create a fresh global name.
freshGlobal ::
  ( HasState "globals" (GlobalMap primTy primVal) m,
    HasState "nameGen" Natural m
  ) =>
  m NameSymbol.T
freshGlobal = freshGlobalName "global"

-- | Create a fresh global name from a given basis.
freshGlobalName ::
  ( HasState "globals" (GlobalMap primTy primVal) m,
    HasState "nameGen" Natural m
  ) =>
  NameSymbol.T ->
  m NameSymbol.T
freshGlobalName base = do
  nameGen <- get @"nameGen"
  globals <- get @"globals"
  let names = Map.keys globals
      newNames =
        [ (n', base `appendNameSymbol` (NameSymbol.fromText $ show n))
          | n' <- [nameGen :: Natural ..]
        ]
      ((n, name') : _) = filter (\(_, x) -> not $ elem x names) newNames
  put @"nameGen" (n + 1)
  return name'

-- | 'Dumb' append of two @NameSymbol.T@, i.e. it does not add a dividing @.@
-- in between for Juvix module scoping.
appendNameSymbol :: NameSymbol.T -> NameSymbol.T -> NameSymbol.T
appendNameSymbol ns1 ns2 = NameSymbol.fromSymbol $ intern $ ns1' <> ns2'
  where
    ns1' = S.fromString $ unintern $ NameSymbol.toSymbol ns1
    ns2' = S.fromString $ unintern $ NameSymbol.toSymbol ns2
