module Juvix.Context.InfoHelper (injectNewTerm, injectMetaInformation) where

import Control.Lens (over, set, (^.))
import qualified Juvix.Context as Context
import qualified Juvix.Context.NameSpace as NameSpace
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Sexp as Sexp

-- | @injectNewTerm@ injects the current definition into the current
-- module by the name, preserving all the meta information that may
-- already be there.
injectNewTerm :: Context.T -> Symbol -> Sexp.T -> Context.T
injectNewTerm ctx name def =
  let newTerm =
        case Context.lookup (pure name) ctx of
          Just info
            -- If it comes from the outside then it's not the same
            -- symbol we are adding here, since it's a local name we
            -- are adding.
            | (info ^. Context.nameSpace) /= Context.Outside ->
              set Context.def (Context.Term def) (info ^. Context.term)
          _ ->
            Context.Info mempty (Context.Term def)
   in Context.add (NameSpace.Pub name) newTerm ctx

-- | @injectMetaInformation@ injects the current definition into the
-- current module, keeping the term, but changing the meta-table by
-- the values in the tuple (key, value).
injectMetaInformation :: Context.T -> Symbol -> (Symbol, Sexp.T) -> Context.T
injectMetaInformation ctx name (metaName, value) =
  let newTerm =
        case Context.lookup (pure name) ctx of
          Just info
            -- If it comes from the outside then it's not the same
            -- symbol we are adding here, since it's a local name we
            -- are adding.
            | (info ^. Context.nameSpace) /= Context.Outside ->
              over Context.table (HashMap.insert metaName value) (info ^. Context.term)
          _ ->
            Context.Info
              (HashMap.singleton metaName value)
              (Context.Term (Sexp.serialize @Text ":empty"))
   in Context.add (NameSpace.Pub name) newTerm ctx
