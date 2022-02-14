{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

-- | Calculate mutually-recursive groups of definitions.
module Juvix.Core.Common.Context.Traverse
  ( traverseContext,
    traverseContext1,
    traverseContext_,
    traverseContext1_,
    Entry (..),
    Group,
    Groups,
    recGroups,
  )
where

import Control.Lens hiding ((|>))
import qualified Data.Graph as Graph
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.NonEmpty.Extra as NonEmpty
import qualified Generics.SYB as SYB
import qualified Juvix.Context as Context
import qualified Juvix.Context.NameSpace as NameSpace
import Juvix.Core.Common.Context.Traverse.Types
import qualified Juvix.FreeVars as FV
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp.Structure.Lens as L
import qualified Juvix.Sexp.Structure.Transition as Structure

-- | Traverses a whole context by performing an action on each recursive group.
-- The groups are passed in dependency order but the order of elements within
-- each group is arbitrary.
traverseContext ::
  (Applicative f, Monoid t) =>
  -- | process one recursive group
  (Group -> f t) ->
  Context.T ->
  f t
traverseContext f = foldMapA f . recGroups

-- | As 'traverseContext' but ignoring the return value.
traverseContext_ ::
  Applicative f =>
  -- | process one recursive group
  (Group -> f z) ->
  Context.T ->
  f ()
traverseContext_ f = traverse_ f . recGroups

-- | Same as 'traverseContext', but the groups are split up into single
-- definitions.
traverseContext1 ::
  (Applicative f, Monoid t) =>
  -- | process one definition
  (NameSymbol.T -> Context.Definition -> f t) ->
  Context.T ->
  f t
traverseContext1 = traverseContext . foldMapA . onEntry

-- | Same as 'traverseContext1', but ignoring the return value.
traverseContext1_ ::
  Applicative f =>
  -- | process one definition
  (NameSymbol.T -> Context.Definition -> f z) ->
  Context.T ->
  f ()
traverseContext1_ = traverseContext_ . traverse_ . onEntry

onEntry ::
  (NameSymbol.T -> Context.Definition -> t) ->
  Entry ->
  t
onEntry f Entry {name, def} = f name def

-- | Sorts a context by dependency order. Each element of the output is
-- a mutually-recursive group, whose elements depend only on each other and
-- elements of previous groups. The first element of each pair is its
-- fully-qualified name.
recGroups :: Context.T -> [Group]
recGroups ctx@Context.T {topLevelMap} =
  let (groups, deps) = run_ ctx $ recGroups' injectTopLevel $ toNameSpace topLevelMap
      get n = maybe [] toList $ HashMap.lookup n deps
      edges = map (\(n, gs) -> (gs, n, get n)) $ HashMap.toList groups
      (g, fromV', _) = Graph.graphFromEdges edges
      fromV v = let (gs, _, _) = fromV' v in gs
   in Graph.topSort g |> reverse |> concatMap fromV |> sortBy orderDatatypes |> groupCons

-- | Join type and data constructors in a single group
groupCons :: Foldable t => t (NonEmpty Entry) -> [Group]
groupCons = fmap snd . foldr f mempty
  where
    f entry@(a NonEmpty.:| _as) acc
      | Context.Term (Structure.toSumCon -> Just tm) <- def a,
        let sumTName = NameSymbol.toSym (tm ^. L.typeOf),
        Just _ <- find (elem sumTName) (fst <$> acc) =
        -- Find if type declar of a data constructor exists
        foldr (g sumTName) mempty acc
      | otherwise = (name a, entry) : acc
      where
        g sumTName (k, v) a
          | sumTName `elem` k = (k, v `NonEmpty.union` entry) : a
          | otherwise = (k, v) : a

-- | Make data type come before data constructor
orderDatatypes :: Group -> Group -> Ordering
orderDatatypes (a NonEmpty.:| _as) (b NonEmpty.:| _bs) = case (def a, def b) of
  -- Bug here, what if we had
  --
  -- (:sum-con foo) and (type foo.Bar.x)
  --
  -- then it'll give the elem call a false ordering....
  --
  -- Thus we use NameSymbol.last and check that against the name
  (Context.Term (Structure.toSumCon -> Just tm), _)
    | NameSymbol.toSym (tm ^. L.typeOf) == NonEmpty.last (name b) ->
      GT
  (_, Context.Term (Structure.toSumCon -> Just tm))
    | NameSymbol.toSym (tm ^. L.typeOf) == NonEmpty.last (name a) ->
      LT
  (_, _) -> EQ

injectTopLevel :: (Semigroup a, IsString a) => a -> a
injectTopLevel name = Context.topLevelName <> "." <> name

recGroups' ::
  HasRecGroups m => (Symbol -> Symbol) -> NameSpace.T Context.Info -> m ()
recGroups' injection ns = do
  defs <-
    concat <$> for (NameSpace.toList1' ns) \(name, term) ->
      case term ^. Context.def of
        Context.Module ns -> do
          contextName <- gets @"context" Context.currentName
          modify @"context"
            ( \ctx ->
                fromMaybe
                  ctx
                  ( Context.lookup (NameSymbol.fromSymbol (injection name)) ctx
                      >>| (^. Context.qualifedName)
                      >>= (`Context.inNameSpace` ctx)
                  )
            )
          recGroups' identity (ns ^. Context.contents)
          modify @"context"
            (\ctx -> fromMaybe ctx (Context.inNameSpace contextName ctx))
          pure []
        Context.CurrentNameSpace -> do
          curNS <- gets @"context" Context.currentNameSpace
          recGroups' identity (curNS ^. Context.record . Context.contents)
          pure []
        _ -> do
          qname <- qualify name
          fvs <-
            case term ^. Context.def of
              Context.Term tm ->
                fv tm
              _ -> pure []
          -- we remove the TopLevel. from fvs as it screws with the
          -- algorithm resolution
          pure [((term ^. Context.def), qname, fmap Context.removeTopName fvs)]
  let (g, fromV, _) = Graph.graphFromEdges defs
  let accum1 xs v =
        let (def, name, ys) = fromV v
         in (xs <> HashSet.fromList ys, Entry {name, def})
  let accum xs vs = let (ys, es) = mapAccumL accum1 [] vs in (xs <> ys, es)
  let (fvs, groups) =
        Graph.scc g
          |> mapAccumL (\xs t -> accum xs (toList t)) HashSet.empty
  addDeps fvs
  for_ groups addGroup

fv :: (ContextReader m, Data a) => a -> m [NameSymbol.T]
fv t = gets @"context" \ctx ->
  SYB.everything (<>) (SYB.mkQ mempty FV.op) t
    |> HashSet.toList
    |> mapMaybe (\name -> Context.lookup name ctx >>| (^. Context.qualifedName))

toNameSpace :: HashMap.T Symbol Context.Info -> NameSpace.T Context.Info
toNameSpace public = NameSpace.T {public, private = mempty}

-- | Add a group to the final output.
addGroup ::
  (ContextReader m, OutputState m, Foldable t) => t Entry -> m ()
addGroup grp = do
  prefix <- prefixM
  case nonEmpty $ toList grp of
    Just grp -> modify @"output" $ HashMap.alter f prefix
      where
        f = Just . maybe [grp] (<> [grp])
    Nothing -> pure ()

-- | Add dependencies on the given names to the current namespace.
addDeps :: (Foldable t, DepsState m, ContextReader m) => t NameSymbol.T -> m ()
addDeps deps = do
  let mods = HashSet.fromList $ map NameSymbol.mod $ toList deps
  let f = Just . maybe mods (HashSet.union mods)
  prefix <- prefixM
  modify @"deps" $ HashMap.alter f prefix

toMod :: NameSymbol.T -> NameSymbol.Mod
toMod = toList

prefixM :: ContextReader m => m NameSymbol.Mod
prefixM = gets @"context" (toMod . Context.currentName)

-- | Qualify a name by the current module prefix.
qualify :: ContextReader m => Symbol -> m NameSymbol.T
qualify n = gets @"context" ((<> pure n) . Context.currentName)
