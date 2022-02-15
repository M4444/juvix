{-# LANGUAGE OverloadedLists #-}

module Juvix.Core.Common.Context.Traverse.Types
  ( -- * Output types
    Entry (..),
    Group,
    Group',
    Groups,
    Groups',
    Prefix (..),
    Deps,

    -- * Capabilities
    Env,
    ContextReader,
    OutputState,
    DepsState,
    HasRecGroups,
    run,
    run_,
  )
where

import qualified Data.DList as D
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Juvix.Context.Types
import qualified Juvix.Context.Types as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- | A definition identified by its fully-qualified name.
data Entry = Entry
  { name :: NameSymbol.T,
    def :: Info
  }
  deriving (Eq, Show, Generic)

-- | A recursive group of definitions, in an arbitrary order.
type Group = NonEmpty Entry

type Group' = D.DList Entry

-- | All recursive groups in a context. Each namespace has its groups in
-- dependency order.
type Groups =
  HashMap NameSymbol.Mod [Group]

type Groups' =
  HashMap NameSymbol.Mod (D.DList Group)

-- | Module name prefix
newtype Prefix = P (D.DList Symbol)

type Deps = HashMap NameSymbol.Mod (HashSet NameSymbol.Mod)

data S = S
  { output :: Groups',
    context :: Context.T,
    deps :: Deps
  }
  deriving (Generic)

type Alias = State S

newtype Env a = Env (Alias a)
  deriving newtype (Functor, Applicative, Monad)
  deriving
    ( HasSource "output" Groups',
      HasSink "output" Groups',
      HasState "output" Groups'
    )
    via StateField "output" Alias
  deriving
    ( HasSource "context" Context.T,
      HasSink "context" Context.T,
      HasState "context" Context.T
    )
    via StateField "context" Alias
  deriving
    ( HasSource "deps" Deps,
      HasSink "deps" Deps,
      HasState "deps" Deps
    )
    via StateField "deps" Alias

type ContextReader =
  HasState "context" Context.T

type OutputState =
  HasState "output" Groups'

type DepsState = HasState "deps" Deps

type HasRecGroups m =
  ( ContextReader m,
    DepsState m,
    OutputState m
  )

run_ ::
  Context.T ->
  Env a ->
  (Groups, Deps)
run_ context act =
  let (_, grps, deps) = run context act in (grps, deps)

run ::
  Context.T ->
  Env a ->
  (a, Groups, Deps)
run context (Env act) =
  let (res, S {output, deps}) = runState act initState
   in (res, toList <$> output, deps)
  where
    initState = S {output = [], context, deps = []}
