-- |
-- - Serves as the context for lower level programs of the =Juvix=
--   Programming Language
-- - This is parameterized per phase which may store the type and
--   term in slightly different ways
module Juvix.Context
  ( module Juvix.Context.Types,
    module Juvix.Context.Precedence,

    -- * Creation
    Juvix.Context.empty,
    emptyRecord,

    -- * Lookup
    lookup,
    tryLookup,
    (!?),
    CantResolve (..),

    -- * Functions on Info
    lookupInfoSexp,
    lookupInfo,
    precedenceOf,

    -- * Local Function

    -- ** Core API
    add,
    lookupCurrent,
    remove,
    includeMod,

    -- ** Additional Functions
    markPrivate,
    publicNames,

    -- * Glocal Functions

    -- ** Core API
    inNameSpace,
    switchNameSpace,
    addGlobal,
    removeGlobal,
    addPathWithValue,

    -- ** Extra API
    persistDefinition,
    AmbiguousDef (..),

    -- * Dealing with Top Level Naming
    addTopName,
    removeTopName,
    qualifySymbolInternal,
    qualifySymbol,
    trueNameofSymbol,

    -- * Table Operations
    Table (..),
    nameFromTable,
    nameSpaceFromTableErr,
    currentRecordContents,

    -- * Operations on From
    extractValue,

    -- * NameSymbol Operations
  )
where

import Control.Lens hiding ((|>))
import qualified Data.HashSet as Set
import qualified Juvix.Context.InfoNames as Info
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
-- Functions Related to Resolution Logic
--
-- NOTE: lookupModulePub IS RECURSIVE ON lookup
--------------------------------------------------------------------------------

------------------------------------------------------------
-- NOTE: RULES FOR OPEN ARE NOΤ HERE
-- PASSES CURRENTLY HAVE TO RESOLVE THE OPENS THEMSELVES!!!!
-- PLEASE CHANGE THIS IN THE FUTURE
------------------------------------------------------------

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
-- 3. if nameSymbol.head ∈ Included modules, then the relocated Table
--    is given back.
--
-- 4. if currentName = Foo.Bar ∧ nameSymb: Foo.Bar.baz, and 1 and 2 do
--    not hold, then we will look in the public map of the current
--    module, with the newNameSymb being baz in this example
--
-- 5. if none of the above hold, then we give back the global module.
determineTableForFirstLookup :: T -> NonEmpty Symbol -> (Table, NameSymbol.T)
determineTableForFirstLookup t nameSymb =
  let -- Starting Lookup checks
      -------------------------
      name = removeTopName nameSymb
      prefixLook = NameSymbol.takePrefixOf (t ^. _currentName) name
      --
      nameWithoutCurrentPath@(firstSymbol :| _) = fromMaybe name prefixLook
      --
      lookupPriv = NameSpace.lookupPrivate firstSymbol (t ^. currentRecordContents)
      lookupPubi = NameSpace.lookup firstSymbol (t ^. currentRecordContents)
      lookupRelo = lookupModulePub t firstSymbol (t ^. currentRecord)

      -- relevant map information
      ---------------------------
      nameSpace
        | NameSymbol.hd nameSymb == topLevelName && isNothing prefixLook = Outside
        | isJust lookupPriv = Private
        | isJust lookupPubi = Public
        | isJust lookupRelo = Public
        | otherwise =
          case prefixLook of
            -- currentName : Foo.Bar, symbol: Foo.Bar.baz,
            Just __ -> Public
            Nothing -> Outside
      table =
        case nameSpace of
          Outside -> Global nameSpace (t ^. _topLevelMap)
          Private -> Local nameSpace (t ^. currentRecordContents)
          Public ->
            case lookupRelo of
              Just rel@Relocated {} ->
                rel
              ____ -> Local nameSpace (t ^. currentRecordContents)
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
--
-- It is debatable what is better behavior, it might be good to always
-- be local unless toplevel is given, or if we find it in the toplevel
-- map at first, this can be tweaked by the code that calls the
-- context, rather than the context itself. - Mariari
determineTableForFirstModification :: T -> NonEmpty Symbol -> (Table, NameSymbol.T)
determineTableForFirstModification t nameSymb =
  let (table, nameSymbol@(x :| symb)) = determineTableForFirstLookup t nameSymb
   in case (table, symb) of
        (Global _ table, [])
          -- Rule 1. in the docs above
          | not (isJust (HashMap.lookup x table)) && NameSymbol.hd nameSymb /= topLevelName ->
            (Local Public (t ^. currentRecordContents), nameSymbol)
        _ -> (table, nameSymbol)

-- | @lookupModulePub@ looks up the symbol from the current
-- Module. The rule for lookup is as follows.
--
-- 1. Lookup in the local table. If it's there we return
--
-- 2. Lookup in the include table, if that module has the symbol,
-- return Relocated
--
-- 3. If it's Not in either return Nothing.
--
-- Note :: A much simpler implementation would be having a cached
-- table of symbols. Since we don't, we have to lookup each include
-- and can't even do ambiguity checking due to this poor design.
lookupModulePub :: T -> Symbol -> Module -> Maybe Table
lookupModulePub t symbol mod =
  case NameSpace.lookup symbol (mod ^. contents) of
    Just __ -> Local Public (mod ^. contents) |> Just
    Nothing ->
      mod ^. includeList
        >>| findSymbolInInclude (Set.singleton (t ^. _currentName))
        |> asum
      where
        findSymbolInInclude excludeSet nameSymb
          | not (nameSymb `Set.member` excludeSet) = do
            from <- lookup nameSymb t
            mod <- case from ^. term . def of
              Module mod -> Just mod
              __________ -> Nothing
            let foundLocally = do
                  void (NameSpace.lookup symbol (mod ^. contents))
                  Relocated Public (from ^. qualifedName) (mod ^. contents)
                    |> pure
            --
            foundLocally <|> includeCheck (Set.insert nameSymb excludeSet) mod
          | otherwise =
            Nothing
        includeCheck set mod =
          -- lazyness of asum saves us here, in terms of overhead
          mod ^. includeList
            >>| findSymbolInInclude set
            |> asum @_ @Maybe

-- couldn't figure out how to fold lenses
-- once we figure out how to do a fold like
-- foldr (\x y -> x . contents . T  . y) identity brokenKey
-- replace the recursive function with that

-- TODO ∷ add something like
-- checkGlobal
--   | NameSymbol.subsetOf currentName nameSymb

data CantResolve = Cant
  { symbolsLeft :: Maybe NameSymbol.T,
    pathUntilNoResolve :: NameSymbol.T
  }
  deriving (Show)

-- | @tryPathLookup@ tries to grab the path and value of the given
-- @NameSymbol.T@. The first argument determines if the lookup ought
-- to be used for modification (additions), or for querying (lookup,
-- removal), thus one should pass @determineTableForFirstModification@
-- for the first task, and @determineTableForFirstLookup@ for the
-- latter.
tryPathLookup ::
  (T -> NameSymbol.T -> (Table, NameSymbol.T)) ->
  Bool ->
  NameSymbol.T ->
  T ->
  Either CantResolve From
tryPathLookup determineTableForFirstTask canGlobalLookup nameSymb t = do
  let (table, newNameSymb@(x :| symb)) = determineTableForFirstTask t nameSymb
      -- From Instantiation
      ---------------------
      nameSpace = nameFromTable table
      currName = t ^. _currentName
      fullyQualifiedName = qualifySymbolInternal t nameSpace newNameSymb
      form =
        case table of
          Global _ table
            | canGlobalLookup ->
              recursivelyLookup symb (HashMap.lookup x table) topLevelName x
          Local Private tbl ->
            recursivelyLookup symb (NameSpace.lookupPrivate x tbl) (addTopName currName) x
          Local Public tabl ->
            recursivelyLookup symb (NameSpace.lookup x tabl) (addTopName currName) x
          Relocated _ nm tbl ->
            recursivelyLookup symb (NameSpace.lookup x tbl) (addTopName nm) x
          _________________ ->
            Lib.Left Cant {symbolsLeft = Nothing, pathUntilNoResolve = newNameSymb}
  (infoTerm, trueName) <- form
  pure (From nameSpace fullyQualifiedName trueName infoTerm)
  where
    -- In the code below since @determineTableForFirstLookup@ ensure
    -- the path does not have @CurrentNameSpace@, except with the
    -- following exceptions
    --
    -- 1.  except when empty as if it's precisely the same then it
    -- will have it - Mariari
    --
    -- 2. except when a module includes its parent, then it can loop
    -- around to being current
    recursivelyLookup [] mterm currentPath currentLookupName
      | mterm ^? _Just . def == Just CurrentNameSpace =
        Lib.Right
          ( infoRecordToInfo (t ^. _currentNameSpace),
            -- DON'T USΕ (t ^. _currentName) as in the case of
            -- switching, the fact that
            --
            -- currentPath <> pure currentLookupName == (t ^. _currentName)
            --
            -- does not hold
            currentPath <> pure currentLookupName
          )
      | otherwise =
        case mterm of
          Nothing ->
            Lib.Left
              Cant
                { symbolsLeft = Just (currentLookupName :| []),
                  pathUntilNoResolve = currentPath
                }
          Just term ->
            Lib.Right (term, currentPath <> pure currentLookupName)
    recursivelyLookup (x : xs) maybeterm currentPath currentLookupName =
      let cantResolveErr =
            Lib.Left
              Cant
                { symbolsLeft = Just (currentLookupName :| x : xs),
                  pathUntilNoResolve = currentPath
                }
          currentPathWithCurrent = currentPath <> pure currentLookupName
          lookupCurrent table = NameSpace.lookup x table
          lookupNext table =
            case lookupModulePub t x table of
              Just (Local _ ns) ->
                recursivelyLookup xs (lookupCurrent ns) currentPathWithCurrent x
              Just (Relocated _ nm tbl) ->
                -- do not add current lookupname, as the @nm@ has already have it!
                recursivelyLookup xs (lookupCurrent tbl) nm x
              _ ->
                recursivelyLookup xs Nothing currentPathWithCurrent x
       in case maybeterm ^? _Just . def of
            Just (Module module') -> lookupNext module'
            -- This can only happen when a module opened its parent,
            -- and we are pathing back in
            Just CurrentNameSpace -> lookupNext (t ^. _currentNameSpace . record)
            _____________________ -> cantResolveErr

tryLookupGen :: Bool -> NameSymbol.T -> T -> Either CantResolve From
tryLookupGen = tryPathLookup determineTableForFirstLookup

tryModifyPath :: NameSymbol.T -> T -> Either CantResolve From
tryModifyPath = tryPathLookup determineTableForFirstModification True

-- | @modifyWithPath@ modifies the given path in the context. The
-- given name is expected to be a path indexed from the TopLevel to
-- avoid ambiguities. If the name is TopLevel by itself, the function
-- does not run.
modifyWithPath :: NameSymbol.T -> T -> (Info -> Info) -> T
modifyWithPath nameSymbPath t modification = do
  let newNameSymb = removeTopName nameSymbPath
  case NameSymbol.takePrefixOfInternal (t ^. _currentName) newNameSymb of
    Just pathIntenral ->
      t |> over _currentNameSpace (moduleToFunction (recurse pathIntenral))
    Nothing ->
      t |> over _topLevelMap (topSet newNameSymb)
  where
    -- topSet will not run on TopLevel due to how it's formulated.
    topSet (x :| tl) top =
      case lookup' x top of
        Just info ->
          insert' x (recurse tl info) top
        Nothing -> top

    moduleToFunction func mod =
      case func (infoRecordToInfo mod) of
        i@(Info {infoDef = Module _}) -> infoToInfoRecordErr i
        _____________________________ -> mod
    recurse [] table =
      modification table
    recurse (x : xs) info =
      info |> over def (onTableContentsLookup x (insert' x . recurse xs))

onTableContentsLookup ::
  Symbol -> (Info -> NameSpace.T Info -> NameSpace.T Info) -> Definition -> Definition
onTableContentsLookup at func (Module table) =
  case lookup' at (moduleContents table) of
    Just look ->
      table
        |> over contents (func look)
        |> Module
    -- these cases down, just abort, we can't update
    Nothing -> Module table
onTableContentsLookup _ _ otherCases = otherCases

onTableContents ::
  (NameSpace.T Info -> NameSpace.T Info) -> Definition -> Definition
onTableContents func (Module table) =
  table
    |> over contents func
    |> Module
onTableContents _ otherCases = otherCases

--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------

-- | @empty@ creates a starting context with the @NameSymbol.T@ being
-- the starting module.
empty :: NameSymbol.T -> IO T
empty sym = do
  empty <- atomically fullyEmpty
  res <-
    addPathWithValue (addTopName sym) (Info mempty CurrentNameSpace) empty
  case res of
    Lib.Left _ -> error "impossible"
    Lib.Right x -> pure x
  where
    fullyEmpty = do
      currentNameSpace <- emptyRecord
      pure $
        T
          { currentNameSpace = InfoRecord mempty currentNameSpace,
            currentName = removeTopName sym,
            topLevelMap = HashMap.empty,
            reverseLookup = HashMap.empty
          }

emptyRecord :: STM Module
emptyRecord = do
  emptyQualificationMap <- STM.new
  pure
    Mod
      { moduleContents = NameSpace.empty,
        moduleOpenList = [],
        moduleIncludeList = [],
        moduleQualifiedMap = emptyQualificationMap
      }

data AmbiguousDef = AmbiguousDef NameSymbol.T

-- | @persistDefinition@ states that the definition we are adding is
-- staying in the context, so promote it.  In the future this will be
-- called for all definitions added by default, with private
-- definitions not added. However this infra is not up, so the pass
-- adding functions must add it themselves
persistDefinition :: T -> NameSymbol.T -> Symbol -> IO (Either AmbiguousDef ())
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

-- | @lookupCurrent@ Looks up a name in the current module only.
lookupCurrent :: NameSymbol.T -> T -> Maybe From
lookupCurrent tm = rightToMaybe . tryLookupGen False tm

-- TODO ∷ Maybe change
-- By default add adds it to the public map by default!
add :: NameSpace.From Symbol -> Info -> T -> T
add sy term = over currentRecordContents (NameSpace.insert sy term)

-- | @remove@ removes the supplied name from the Context.
remove ::
  NameSpace.From Symbol -> T -> T
remove sy = over currentRecordContents (NameSpace.remove sy)

-- | @markPrivate@ marks the given @Symbol@ as Private.
markPrivate ::
  Symbol -> T -> T
markPrivate sy = over currentRecordContents (NameSpace.markPrivate sy)

-- | @publicNames@ lists the public names of the current module
publicNames :: T -> [Symbol]
publicNames t =
  let NameSpace.List {publicL} = toList t
   in fst <$> publicL

-- | @publicNames@ lists all the names of the current module
toList :: T -> NameSpace.List Info
toList t = NameSpace.toList (t ^. currentRecordContents)

-- | @includeMod@ Includes the given module to the current namespace
includeMod :: NameSymbol.T -> T -> T
includeMod nameSymbol =
  over (_currentNameSpace . record . includeList) (nameSymbol :)

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

-- | @tryLookup@ Tries to lookup the given @NameSymbol.T@ from the
-- Context. If the lookup fails, the last known module is dumped along
-- with what symbol path could not be resolved.
tryLookup :: NameSymbol.T -> T -> Either CantResolve From
tryLookup = tryLookupGen True

-- | @lookup@ acts like @tryLookup@ but does not dump any path
-- information
lookup :: NameSymbol.T -> T -> Maybe From
lookup nm = rightToMaybe . tryLookup nm

-- | @!?@ is just an alias for lookup
(!?) :: T -> NameSymbol.T -> Maybe From
(!?) = flip lookup

-- | @addGlobal@ Adds the given @Info@ to the path supplied by
-- @NameSymbol.T@. @addGlobal@ will try to figure out the best place
-- to add the definition given the rules of scoping. Note that this
-- module does not create nested pathing, so if the module does not
-- exist, this function acts as the identity.
addGlobal :: NameSymbol.T -> Info -> T -> T
addGlobal name info t =
  case tryModifyPath name t of
    Lib.Right from ->
      let fullTrue = from ^. trueName
          symbolAtPoint = NameSymbol.base fullTrue
       in case NameSymbol.mod fullTrue of
            (tm : term) -> addingAtPoint (tm :| term) symbolAtPoint
            [] -> t
    Lib.Left cantResolve ->
      case symbolsLeft cantResolve of
        Just (lastPartOfName :| []) ->
          addingAtPoint (pathUntilNoResolve cantResolve) lastPartOfName
        _ ->
          t
  where
    addingAtPoint path symbolToAdd =
      case path of
        (top :| [])
          | top == topLevelName ->
            over _topLevelMap (insert' symbolToAdd info) t
        mod ->
          modifyWithPath mod t (insertPoint symbolToAdd)
    insertPoint onPoint =
      over def (onTableContents (insert' onPoint info))

-- | @removeGlobal@ removes the given @NameSymbol.T@ from the
-- Context. This should remove the symbol found via !?. If nothing is
-- removed the original context is given back.
removeGlobal :: NameSymbol.T -> T -> T
removeGlobal name t =
  case tryModifyPath name t of
    Lib.Right from -> do
      let fullTrue = from ^. trueName
          symbolAtPoint = NameSymbol.base fullTrue
      case NameSymbol.mod fullTrue of
        (tm : term) -> addingAtPoint (tm :| term) symbolAtPoint
        [] -> t
    Lib.Left _ -> t
  where
    addingAtPoint path symbolToRemove =
      case path of
        (top :| [])
          | top == topLevelName ->
            removeTop symbolToRemove t
        mod ->
          modifyWithPath mod t (removePoint symbolToRemove)
    removePoint onPoint =
      over def (onTableContents (remove' onPoint))

-- | @addPathWithValue@ acts like @addGlobal@ however, instead of just
-- acting like the identify when the module above the term is found,
-- it will add the path. This may result in a failure if the path is
-- already used by another module and or term.
addPathWithValue :: NameSymbol.T -> Info -> T -> IO (Either PathError T)
addPathWithValue name info t =
  case tryModifyPath name t of
    Lib.Right _value ->
      pure (Lib.Left (VariableShared name))
    Lib.Left cantResolve
      | pathUntilNoResolve cantResolve == topLevelName,
        Just left <- symbolsLeft cantResolve -> do
        --
        body cantResolve left
      | Just tm <- t !? pathUntilNoResolve cantResolve,
        Just left <- symbolsLeft cantResolve,
        isModule (tm ^. term . def) -> do
        --
        body cantResolve left
      | otherwise ->
        pure (Lib.Left (VariableShared name))
  where
    body cantResolve (termToInsert :| rest) = do
      nestedTerm <- foldM createPathing info (reverse rest)

      pure (Lib.Right (addGlobal (pathUntilNoResolve cantResolve <> pure termToInsert) nestedTerm t))
    createPathing buildUp nameOfNextModuleNesting = do
      nextMod <- emptyModule
      nextMod
        |> over contents (insert' nameOfNextModuleNesting buildUp)
        |> Module
        |> Info mempty
        |> pure
    isModule (Module _) = True
    isModule __________ = False

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
    (addTopName currentName)
    (infoRecordToInfo currentNameSpace)

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

lookupInfoSexp :: Info -> Symbol -> Maybe Sexp.T
lookupInfoSexp info sym =
  HashMap.lookup sym (info ^. table)

lookupInfo :: forall a. Sexp.Serialize a => Info -> Symbol -> Maybe a
lookupInfo info sym =
  HashMap.lookup sym (info ^. table) >>= Sexp.deserialize @a

-- TODO ∷ abstract out the name precedence
precedenceOf :: Info -> Maybe Precedence
precedenceOf info = lookupInfo info Info.precedence

-------------------------------------------------------------------------------
-- Generalized Helpers
-------------------------------------------------------------------------------

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

data Table
  = -- namespace has to be Outisde in the Global Case
    Global NameSpace (HashMap.T Symbol Info)
  | -- namespace can not be Outside in the Local Case
    Local NameSpace (NameSpace.T Info)
  | -- @Relocated@ represents the module to look at first
    Relocated NameSpace NameSymbol.T (NameSpace.T Info)
  deriving (Show)

nameFromTable :: Table -> NameSpace
nameFromTable (Global name ______) = name
nameFromTable (Local name _______) = name
nameFromTable (Relocated name _ _) = name

nameSpaceFromTableErr :: Table -> NameSpace.T Info
nameSpaceFromTableErr (Local _______ ns) = ns
nameSpaceFromTableErr (Relocated _ _ ns) = ns
nameSpaceFromTableErr (Global _ _______) = error "called nameSpaceFromTableErr on Global"

currentRecordContents ::
  Functor f => (NameSpace.T Info -> f (NameSpace.T Info)) -> T -> f T
currentRecordContents = _currentNameSpace . record . contents

currentRecord :: Functor f => (Module -> f Module) -> T -> f T
currentRecord = _currentNameSpace . record

-- | @qualifySymbol@ returns the qualified name of the symbol. It does
-- not return the TRUENAME of the module. Thus
--
-- λ> qualifySymbol Ctx@{current = Foo.Bar} (Relocated ... Baz.Chaz ...) "x"
-- > TopLevel.Foo.Bar.x
--
-- The Truename of x is
-- Toplevel.Baz.Chaz.x
--
-- Thus we get TopLevel.Foo.Bar.x and not Toplevel.Baz.Chaz.x
qualifySymbol :: T -> NameSpace -> NameSymbol.T -> NameSymbol.T
qualifySymbol t namespace symbol =
  let (_, newNameSymb) = determineTableForFirstLookup t symbol
  in qualifySymbolInternal t namespace newNameSymb


qualifySymbolInternal :: T -> NameSpace -> NameSymbol.T -> NameSymbol.T
qualifySymbolInternal _ Outside = addTopName
qualifySymbolInternal t Private = (addTopName (t ^. _currentName) <>) -- decide on private res!!!
qualifySymbolInternal t Public = (addTopName (t ^. _currentName) <>)


trueNameofSymbol :: T -> NameSymbol.T -> Maybe NameSymbol.T
trueNameofSymbol ctx n =
  fmap (^. trueName) (ctx !? n)
