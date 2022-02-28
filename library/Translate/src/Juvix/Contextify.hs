module Juvix.Contextify (ResolveErr (..), PathError, resolveOpens, RunM, M (..)) where

import qualified Juvix.Context as Context
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as ResolveOpen
import qualified Juvix.Contextify.ToContext.Sexp as ContextSexp
import qualified Juvix.Contextify.ToContext.Types as Contextify
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

type RunM =
  ExceptT Context.PathError IO

newtype M a = M (RunM a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasThrow "left" Context.PathError) via MonadError RunM

data ResolveErr
  = Path Context.PathError
  | Resolve ResolveOpen.Error
  | PassErr Sexp.T
  deriving (Show, Eq, Generic)

instance Sexp.DefaultOptions ResolveErr

instance Sexp.Serialize ResolveErr

type PathError t = Either Context.PathError t

--------------------------------------------------------------------------------
-- Main functionality
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- we get the opens
resolveOpens ::
  (MonadIO m, HasThrow "left" Context.PathError m) =>
  (Context.T, [ResolveOpen.PreQualified]) ->
  (NameSymbol.T, [Sexp.T]) ->
  m (Context.T, [ResolveOpen.PreQualified])
resolveOpens (ctx', openList) (sym, xs) = do
  ctx <- ContextSexp.run ctx' (sym, xs)
  case ctx of
    Right Contextify.PS {ctxS, opensS, modsDefinedS} -> do
      pure
        ( ctxS,
          ResolveOpen.Pre
            { opens = opensS,
              explicitModule = sym,
              implicitInner = modsDefinedS
            } :
          openList
        )
    Left err ->
      throw @"left" err
