-- |
-- - Serves as the context for lower level programs of the =Juvix=
--   Programming Language
-- - This is parameterized per phase which may store the type and
--   term in slightly different ways
module Juvix.Context
  ( module Juvix.Context.Types,
    module Juvix.Context.Precedence,
    -- leave the entire module for now, so lenses can be exported
    module Juvix.Context,
  )
where

import Control.Lens hiding ((|>))
import qualified Juvix.Context.NameSpace as NameSpace
import Juvix.Context.Precedence
import Juvix.Context.Types
import Juvix.Library hiding (Sum, modify, toList)
import qualified Juvix.Library as Lib
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import qualified StmContainers.Map as STM
import Prelude (error)

--------------------------------------------------------------------------------
-- In lieu of not being able to export namespaces
--------------------------------------------------------------------------------
type NameSymbol = NameSymbol.T

nameSymbolToSymbol :: NameSymbol.T -> Symbol
nameSymbolToSymbol = NameSymbol.toSymbol

nameSymbolFromSymbol :: Symbol -> NameSymbol.T
nameSymbolFromSymbol = NameSymbol.fromSymbol

--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

-- |
empty :: NameSymbol.T -> IO T
empty sym = do
  empty <- atomically fullyEmpty
  res <-
    addPathWithValue (pure topLevelName <> sym') (Info mempty CurrentNameSpace) empty
  case res of
    Lib.Left _ -> error "impossible"
    Lib.Right x -> pure x
  where
    fullyEmpty = do
      currentNameSpace <- emptyRecord
      pure $
        T
          { currentNameSpace = InfoRecord mempty currentNameSpace,
            currentName = sym',
            topLevelMap = HashMap.empty,
            reverseLookup = HashMap.empty
          }
    sym' = removeTopName sym

qualifyName :: NameSymbol.T -> T -> NameSymbol.T
qualifyName sym T {currentName} = currentName <> sym

emptyRecord :: STM Module
emptyRecord = do
  emptyQualificationMap <- STM.new
  pure
    Mod
      { moduleContents = NameSpace.empty,
        moduleOpenList = [],
        moduleQualifiedMap = emptyQualificationMap
      }

data AmbiguousDef = AmbiguousDef NameSymbol.T

-- | @persistDefinition@ states that the definition we are adding is
-- staying in the context, so promote it.  In the future this will be
-- called for all definitions added by default, with private
-- definitions not added. However this infra is not up, so the pass
-- adding functions must add it themselves
persistDefinition ::
  T -> NameSymbol.T -> Symbol -> IO (Either AmbiguousDef ())
persistDefinition T {reverseLookup} moduleName name =
  case HashMap.lookup moduleName reverseLookup of
    Just xs -> do
      determined <- determineSafe xs
      case determined of
        Lib.Left err -> pure $ Lib.Left err
        Lib.Right () -> Lib.Right <$> traverse_ f xs
      where
        -- TODO ∷ replace with a breaking foldM later
        -- this isn't in as I have to do IO actions
        determineSafe [] = pure (Lib.Right ())
        determineSafe (Who {modName, symbolMap} : xs) = do
          lookd <- atomically $ STM.lookup name symbolMap
          case lookd of
            Nothing -> determineSafe xs
            Just SymInfo {mod}
              | mod == moduleName -> determineSafe xs
              | otherwise -> pure (Lib.Left (AmbiguousDef modName))
        -- we do a check before this so the call is always safe
        f Who {symbolMap} = atomically do
          lookd <- STM.lookup name symbolMap
          case lookd of
            Nothing ->
              STM.insert (SymInfo NotUsed moduleName) name symbolMap
            -- TODO ∷ we may change behavior here based on implicit vs explicit
            Just SymInfo {} -> pure ()
    Nothing -> pure (Lib.Right ())

--------------------------------------------------------------------------------
-- Functions on the Current NameSpace
--------------------------------------------------------------------------------

lookupCurrent ::
  NameSymbol.T -> T -> Maybe From
lookupCurrent = lookupGen False

-- TODO ∷ Maybe change
-- By default add adds it to the public map by default!
add :: NameSpace.From Symbol -> Info -> T -> T
add sy term = over currentRecordContents (NameSpace.insert sy term)

addNoMeta :: NameSpace.From Symbol -> Definition -> T -> T
addNoMeta sy = add sy . Info mempty

remove ::
  NameSpace.From Symbol -> T -> T
remove sy = over currentRecordContents (NameSpace.remove sy)

publicNames :: T -> [Symbol]
publicNames t =
  let NameSpace.List {publicL} = toList t
   in fst <$> publicL

toList :: T -> NameSpace.List Info
toList t = NameSpace.toList (t ^. currentRecordContents)

topList :: T -> [(Symbol, Info)]
topList T {topLevelMap} = HashMap.toList topLevelMap

--------------------------------------------------------------------------------
-- Global Functions
--------------------------------------------------------------------------------

-- we lose some type information here... we should probably reserve it somehow

-- | inNameSpace works like in-package in CL
-- we simply just change from our current namespace to another
inNameSpace :: NameSymbol.T -> T -> Maybe T
inNameSpace newNameSpace t@T {currentName}
  | removeTopName newNameSpace == removeTopName currentName =
    pure t
  | otherwise = do
    from <- t !? newNameSpace
    case from ^. term . def of
      Module {} ->
        from ^. qualifedName
          |> changeCurrentModuleWith t (infoToInfoRecordErr (from ^. term))
          |> Just
      _ ->
        Nothing

-- | switchNameSpace works like a mixture of defpackage and in-package from CL
-- creating a namespace if not currently there, and switching to it
-- this function may fail if a path is given like `Foo.Bar.x.f` where x
-- is a non record.
-- This function also keeps the invariant that there is only one CurrentNameSpace
-- tag
switchNameSpace :: NameSymbol.T -> T -> IO (Either PathError T)
switchNameSpace newNameSpace t@T {currentName}
  | removeTopName newNameSpace == removeTopName currentName =
    pure (Lib.Right t)
  | otherwise = do
    -- We do some repeat work here, namely we allocate an empty record
    -- in case we need to place it in for the new namespace
    empty <- atomically emptyRecord
    -- addPathWithValue will create new name spaces if they don't
    -- already exist all the way to where we need it to be
    newT <-
      addPathWithValue newNameSpace (Info mempty (Module empty)) t
        >>| \case
          Lib.Right t -> t
          Lib.Left {} -> t
    -- here we repeat work,
    -- if we successfully added the value, then we have to go down the
    -- same path to retrieve the module, hence duplicating the work
    pure $ case inNameSpace newNameSpace newT of
      Nothing -> Lib.Left (VariableShared newNameSpace)
      Just ct -> Lib.Right ct

lookup :: NameSymbol.T -> T -> Maybe From
lookup = lookupGen True

(!?) :: T -> NameSymbol.T -> Maybe From
(!?) = flip lookup

modifyGlobal :: NameSymbol.T -> (Maybe Info -> Info) -> T -> T
modifyGlobal sym g t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final x) =
      UpdateNow (g x)
    f (Continue (Just info@Info {infoDef = Module {}})) =
      GoOn (infoToInfoRecordErr info)
    f (Continue _) =
      Abort

addGlobal :: NameSymbol.T -> Info -> T -> T
addGlobal sym def t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final _) =
      UpdateNow def
    f (Continue (Just info@Info {infoDef = Module {}})) =
      GoOn (infoToInfoRecordErr info)
    f (Continue _) =
      Abort

addPathWithValue :: NameSymbol.T -> Info -> T -> IO (Either PathError T)
addPathWithValue sym def t = do
  ret <- modifySpaceImp f sym t
  case ret of
    Just tt -> pure (Lib.Right tt)
    Nothing -> pure (Lib.Left (VariableShared sym))
  where
    f (Final Nothing) = pure (UpdateNow def)
    f (Final (Just _)) = pure Abort
    f (Continue Nothing) =
      atomically STM.new
        >>| GoOn . InfoRecord mempty . Mod NameSpace.empty []
    f (Continue (Just info@Info {infoDef = Module {}})) =
      GoOn (infoToInfoRecordErr info) |> pure
    f (Continue _) =
      pure Abort

removeNameSpace :: NameSymbol -> T -> T
removeNameSpace sym t =
  case modifySpace f sym t of
    Just tt -> tt
    Nothing -> t
  where
    f (Final (Just _)) =
      RemoveNow
    f (Final Nothing) =
      Abort
    f (Continue (Just info@Info {infoDef = Module {}})) =
      GoOn (infoToInfoRecordErr info)
    f (Continue Nothing) =
      Abort
    f (Continue (Just _)) =
      Abort

removeTop :: Symbol -> T -> T
removeTop sym t@T {topLevelMap} =
  t {topLevelMap = HashMap.delete sym topLevelMap}

------------------------------------------------------------
-- Helpers for switching the global NameSpace
------------------------------------------------------------

-- | 'changeCurrentModuleWith' moves the current name space and inserts
-- the new namespace as the top
changeCurrentModuleWith :: T -> InfoRecord -> NonEmpty Symbol -> T
changeCurrentModuleWith t startingContents newCurrName =
  let queued = queueCurrentModuleBackIn t
   in t
        { currentName = removeTopName newCurrName,
          currentNameSpace = startingContents
        }
        |> queued
        |> addGlobal newCurrName (Info mempty CurrentNameSpace)

queueCurrentModuleBackIn :: T -> T -> T
queueCurrentModuleBackIn T {currentNameSpace, currentName} =
  -- we note that the currentName is a topLevel name so it
  -- gets added from the top and doesn't confuse itself with
  -- a potnentially local insertion
  addGlobal
    (addTopNameToSngle currentName)
    (infoRecordToInfo currentNameSpace)

-- 'putCurrentModuleBackIn' adds the current name space back to the
-- environment map
-- Note that we have to have the path to the module ready and open
-- as 'addGlobal' quits if it can't find the path
-- we HAVE to remove the current module after this

putCurrentModuleBackIn :: T -> T
putCurrentModuleBackIn t = queueCurrentModuleBackIn t t

addTopNameToSngle :: IsString a => NonEmpty a -> NonEmpty a
addTopNameToSngle (x :| []) = topLevelName :| [x]
addTopNameToSngle xs = xs

addTopName :: (IsString a, Eq a) => NonEmpty a -> NonEmpty a
addTopName (x :| xs)
  | topLevelName == x = x :| xs
  | otherwise = topLevelName :| (x : xs)

removeTopName :: (Eq a, IsString a) => NonEmpty a -> NonEmpty a
removeTopName (top :| x : xs)
  | topLevelName == top = x :| xs
removeTopName (top :| [])
  | top == topLevelName = "" :| []
removeTopName xs = xs

-------------------------------------------------------------------------------
-- Functions on From
-------------------------------------------------------------------------------

extractValue :: From -> Info
extractValue from = from ^. term

-------------------------------------------------------------------------------
-- Functions on Info
-------------------------------------------------------------------------------

lookupInfo :: forall a. Sexp.Serialize a => Info -> Symbol -> Maybe a
lookupInfo info sym =
  HashMap.lookup sym (info ^. table) >>= Sexp.deserialize @a

precedenceOf :: Info -> Maybe Precedence
precedenceOf info = lookupInfo info "precedence"

-------------------------------------------------------------------------------
-- Generalized Helpers
-------------------------------------------------------------------------------

----------------------------------------
-- Types for Generalized Helpers
----------------------------------------

data Stage b
  = -- | 'Final' signifies the last symbol that we
    -- pursue in updating a structure
    Final b
  | -- | 'Continue' signifies that there are parts of
    -- the namespace that we can still continue down
    Continue b

data Return
  = -- | 'GoOn' signifies that we should continue
    -- going down records
    GoOn InfoRecord
  | -- | 'Abort' signifies that we should cancel
    -- the changes on the map and
    Abort
  | -- | 'UpdateNow' signifies that we should
    -- update the context with the current value
    UpdateNow Info
  | -- | 'RemoveNow' signifies that we show remove
    -- the definition at this level
    RemoveNow

----------------------------------------
-- Type Class for Genralized Helpers
----------------------------------------

-- Type class for removing repeat code
class MapSym m where
  lookup' :: Symbol -> m a -> Maybe a
  remove' :: Symbol -> m a -> m a
  insert' :: Symbol -> a -> m a -> m a

instance MapSym (HashMap.T Symbol) where
  lookup' = HashMap.lookup
  remove' = HashMap.delete
  insert' = HashMap.insert

instance MapSym NameSpace.T where
  lookup' = NameSpace.lookup
  remove' = NameSpace.removePublic
  insert' x = NameSpace.insert (NameSpace.Pub x)

newtype PrivNameSpace v = Priv {unPriv :: NameSpace.T v}

instance MapSym PrivNameSpace where
  lookup' sym = NameSpace.lookup sym . unPriv
  remove' sym = Priv . NameSpace.removePrivate sym . unPriv
  insert' sym def = Priv . NameSpace.insert (NameSpace.Priv sym) def . unPriv

modifySpace :: (Stage (Maybe Info) -> Return) -> NameSymbol.T -> T -> Maybe T
modifySpace f symbol t = runIdentity (modifySpaceImp (Identity . f) symbol t)

-- This function dispatches to recurseImp, and serves to deal with
-- giving recurseImp the proper map to run on. this is either the
-- private local, public local, or global
modifySpaceImp ::
  Monad m => (Stage (Maybe Info) -> m Return) -> NameSymbol.T -> T -> m (Maybe T)
modifySpaceImp f symbol t = do
  let (table, newSymb) = determineTableForFirstModification t symbol
  case table of
    Local Private _ ->
      applyAndSetCurrent (fmap (fmap unPriv) . recurseImp f newSymb . Priv)
    Local Public _ ->
      applyAndSetCurrent (recurseImp f newSymb)
    Local Outside _ -> error "impossible"
    Global _ _ ->
      applyAndSetTop (recurseImp f newSymb)
  where
    -- dumb repeat code but idk how to handle to remove the tuple ☹
    applyAndSetTop f =
      t |> overMaybe f (_topLevelMap)
    applyAndSetCurrent f =
      t |> overMaybe f currentRecordContents

-- | @overMaybe@ acts like @over@/@traverseOf@ in lenses, however the
-- function may return a maybe instead of the value, and if that's the
-- case, then the value is not set.
overMaybe ::
  Monad m => (b -> m (Maybe b)) -> Lens s s b b -> s -> m (Maybe s)
overMaybe f field t =
  f (t ^. field)
    >>| fmap (\ret -> set field ret t)

recurseImp ::
  (MapSym map, Monad m) =>
  (Stage (Maybe Info) -> m Return) ->
  NameSymbol.T ->
  map Info ->
  m (Maybe (map Info))
recurseImp f (x :| y : xs) cont = do
  ret <- f (Continue (lookup' x cont))
  case ret of
    GoOn goRecord -> do
      recursed <- recurseImp f (y :| xs) (goRecord ^. record . contents)
      let g newRecord =
            insert' x (goRecord |> set (record . contents) newRecord |> infoRecordToInfo) cont
       in pure (g <$> recursed)
    Abort ->
      pure Nothing
    RemoveNow ->
      pure (Just (remove' x cont))
    UpdateNow newRecord ->
      pure (Just (insert' x newRecord cont))
recurseImp f (x :| []) cont = do
  ret <- f (Final (lookup' x cont))
  case ret of
    UpdateNow return ->
      pure (Just (insert' x return cont))
    RemoveNow ->
      pure (Just (remove' x cont))
    -- GoOn makes no sense here, so act as an Abort
    GoOn {} -> pure Nothing
    Abort -> pure Nothing

-- couldn't figure out how to fold lenses
-- once we figure out how to do a fold like
-- foldr (\x y -> x . contents . T  . y) identity brokenKey
-- replace the recursive function with that

-- TODO ∷ add something like
-- checkGlobal
--   | NameSymbol.subsetOf currentName nameSymb

-- eventually to check if we are referencing an inner module via the top
-- This will break code where you've added local

lookupGen :: Bool -> NameSymbol.T -> T -> Maybe From
lookupGen canGlobalLookup nameSymb t =
  let (table, newNameSymb@(x :| symb)) = determineTableForFirstLookup t nameSymb
      -- From Instantiation
      ---------------------
      nameSpace = nameFromTable table
      fullyQualifiedName = qualifySymbol t nameSpace newNameSymb
      form =
        case table of
          Global _ table
            | canGlobalLookup ->
              recursivelyLookup symb (HashMap.lookup x table)
          Local Private table -> recursivelyLookup symb (NameSpace.lookupPrivate x table)
          Local Public ttable -> recursivelyLookup symb (NameSpace.lookup x ttable)
          ___________________ -> Nothing
   in From nameSpace fullyQualifiedName <$> form
  where
    -- In the code below since @determineTableForFirstLookup@ ensure
    -- the path does not have @CurrentNameSpace@, we ignore that
    -- possibility entirely
    recursivelyLookup [] mterm = mterm
    recursivelyLookup (x : xs) maybeterm =
      let lookupNext table =
            recursivelyLookup xs (NameSpace.lookup x table)
       in case maybeterm ^? _Just . def of
            Just (Module module') -> lookupNext (module' ^. contents)
            _____________________ -> Nothing

-- | @determineTableForFirstLookup@ handles the logic for determining
-- which table should be looked at first. Further resolution is given
-- to make sure that the path does not contain the
-- @CurrentNameSpace@. Lastly the function returns the namesymbol
-- handed with the toplevelname removed if there is one. The
-- following rules are applied:
--
-- 1. if nameSymbol.head ∈ prviate namespace space current name, then
--    the Private Table is given back
--
-- 2. if nameSymbol.head ∈ public namespace space current name, then
--    the Public Table is given back
--
-- 3. if currentName = Foo.Bar ∧ nameSymb: Foo.Bar.baz, and 1 and 2 do
--    not hold, then we will look in the public map of the current
--    module, with the newNameSymb being baz in this example
--
-- 4. if none of the above hold, then we give back the global module.
determineTableForFirstLookup :: T -> NonEmpty Symbol -> (Table, NameSymbol.T)
determineTableForFirstLookup t nameSymb =
  let -- Starting Lookup checks
      -------------------------
      name@(firstSymbol :| _) = removeTopName nameSymb
      lookupPriv = NameSpace.lookupPrivate firstSymbol (t ^. currentRecordContents)
      lookupPubi = NameSpace.lookup firstSymbol (t ^. currentRecordContents)
      prefixLook = NameSymbol.takePrefixOf (t ^. _currentName) name
      -- relevant map information
      ---------------------------
      nameSpace
        | NameSymbol.hd nameSymb == topLevelName && isNothing prefixLook = Outside
        | isJust lookupPriv = Private
        | isJust lookupPubi = Public
        | otherwise =
          case prefixLook of
            -- currentName : Foo.Bar, symbol: Foo.Bar.baz,
            Just __ -> Public
            Nothing -> Outside
      nameWithoutCurrentPath =
        fromMaybe name prefixLook
      table =
        case nameSpace of
          Outside -> Global nameSpace (t ^. _topLevelMap)
          Private -> Local nameSpace (t ^. currentRecordContents)
          Public -> Local nameSpace (t ^. currentRecordContents)
   in (table, nameWithoutCurrentPath)

-- | @determineTableForFirstModification@ handles logic for determine
-- which table should be modified first. The function works just like
-- @determineTableForFirstLookup@ however, the extra rules are as
-- follows
--
-- 1. if the symbol is something like @"Foo"@, then the local map is
-- given and not the global one, IFF the toplevel does not have
-- @"Foo"@
--
-- Note that there is one degenerative case as follows.
--
-- in-module Foo.Bar
-- make-new-module Foo.Bar.Baz
--
-- @
-- cont <- Juvix.Context.empty "Foo.Bar"
-- Juvix.Library.Right cont' <- switchNameSpace "Foo.Bar.Baz" cont
-- λ> currentName cont'
-- "Foo" :| ["Bar","Baz"]
-- @
--
-- Instead of giving the module Foo.Bar.Foo.Bar.Baz, it will give back
-- Foo.Bar.Baz, this means that a function like in-module should check
-- if the @topLevelName@ is in the name, and if not, then insert the
-- currentname with it.
determineTableForFirstModification :: T -> NonEmpty Symbol -> (Table, NameSymbol.T)
determineTableForFirstModification t nameSymb =
  let (table, nameSymbol@(x :| symb)) = determineTableForFirstLookup t nameSymb
   in case (table, symb) of
        (Global _ table, [])
          -- Rule 1. in the docs above
          | not (isJust (HashMap.lookup x table)) ->
            (Local Public (t ^. currentRecordContents), nameSymbol)
        _ -> (table, nameSymbol)

data Table
  = -- namespace has to be Outisde in the Global Case
    Global NameSpace (HashMap.T Symbol Info)
  | -- namespace can not be Outside in the Local Case
    Local NameSpace (NameSpace.T Info)

nameFromTable :: Table -> NameSpace
nameFromTable (Global name _) = name
nameFromTable (Local name _) = name

currentRecordContents ::
  Functor f => (NameSpace.T Info -> f (NameSpace.T Info)) -> T -> f T
currentRecordContents = _currentNameSpace . record . contents

qualifySymbol :: T -> NameSpace -> NameSymbol.T -> NameSymbol.T
qualifySymbol _ Outside = addTopName
qualifySymbol t Private = (addTopName (t ^. _currentName) <>) -- decide on private res!!!
qualifySymbol t Public = (addTopName (t ^. _currentName) <>)
