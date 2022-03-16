{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Juvix.Contextify.Environment
  ( ErrorS (..),
    ErrS,
    Context.T,
    HasClosure,
    contextPassStar,
    extractInformation,
    lookupPrecedence,
    handleAtomNoCtx,
    handleAtom,
    Minimal (..),
    MinimalAlias,
    MinimalAliasIO,
    MinimalM (..),
    MinimalMIO (..),
    Pass,
    PassNoCtx,
    throwSexp,
    runMIO,
    runM,
    namedForms,
    onExpression,
    PassChange (..),
  )
where

import Control.Lens hiding ((|>))
import qualified Juvix.BerlinPipeline.Feedback as Feedback
import qualified Juvix.Closure as Closure
import qualified Juvix.Context as Context
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Context.Traversal as Context
import qualified Juvix.Contextify.Binders as Bind
import qualified Juvix.Contextify.InfixPrecedence.ShuntYard as Shunt
import Juvix.Library hiding (on)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import Prelude (error)

data ErrorS
  = CantResolve [Sexp.T]
  | UnknownSymbol NameSymbol.T
  | ImproperForm Sexp.T
  | ImpossibleMoreEles
  | Clash
      (Shunt.Precedence NameSymbol.T)
      (Shunt.Precedence NameSymbol.T)
  deriving (Show, Eq, Generic)

instance Sexp.DefaultOptions (Shunt.Associativity)

instance Sexp.Serialize (Shunt.Associativity)

instance Sexp.DefaultOptions (Shunt.Precedence NameSymbol.T)

instance Sexp.Serialize (Shunt.Precedence NameSymbol.T)

instance Sexp.DefaultOptions ErrorS

instance Sexp.Serialize ErrorS

type HasClosure m = HasReader "closure" Closure.T m

type ErrS m = (Feedback.Eff m, HasThrow "error" Sexp.T m)

throwSexp :: ErrS m => ErrorS -> m a
throwSexp err = do
  Feedback.error sErr
  throw @"error" sErr
  where
    sErr = (Sexp.serialize err)

type HasSearch m = (ErrS m, HasClosure m)

------------------------------------------------------------
-- Runner environment
------------------------------------------------------------

data Minimal = Minimal
  { closure :: Closure.T,
    feedback :: Feedback.T
  }
  deriving (Generic, Show)

type MinimalAlias =
  ExceptT Sexp.T (State Minimal)

newtype MinimalM a = Ctx {_run :: MinimalAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasReader "closure" Closure.T,
      HasSource "closure" Closure.T
    )
    via ReaderField "closure" MinimalAlias
  deriving
    (HasThrow "error" Sexp.T)
    via MonadError MinimalAlias

type MinimalAliasIO =
  ExceptT Sexp.T (StateT Minimal IO)

newtype MinimalMIO a = CtxIO {_runIO :: MinimalAliasIO a}
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasReader "closure" Closure.T,
      HasSource "closure" Closure.T
    )
    via ReaderField "closure" MinimalAliasIO
  deriving
    ( HasState "feedback" Feedback.T,
      HasSource "feedback" Feedback.T,
      HasSink "feedback" Feedback.T
    )
    via StateField "feedback" MinimalAliasIO
  deriving
    (HasThrow "error" Sexp.T)
    via MonadError MinimalAliasIO

runMIO :: MinimalMIO a -> IO (Either Sexp.T a, Minimal)
runMIO (CtxIO c) = runStateT (runExceptT c) (Minimal Closure.empty Feedback.empty)

runM :: MinimalM a -> (Either Sexp.T a, Minimal)
runM (Ctx c) = runState (runExceptT c) (Minimal Closure.empty Feedback.empty)

------------------------------------------------------------
-- Type aliases
------------------------------------------------------------

type BindSexp a = Sexp.B (Bind.BinderPlus a)

type BindAtom a = Sexp.Atom (Bind.BinderPlus a)

type Pass m a = PassSig m (Bind.BinderPlus a)

type PassNoCtx m a = PassNoCtxSig m (Bind.BinderPlus a)

type PassSig m a =
  Context.T -> Sexp.Atom a -> (Sexp.B a -> m (Sexp.B a)) -> m (Sexp.B a)

type PassNoCtxSig m a =
  Sexp.Atom a -> (Sexp.B a -> m (Sexp.B a)) -> m (Sexp.B a)

-- | @PassChange@ Instead of recursing on the sexp structure, we check
-- the top form and see if it needs to be changed.
newtype PassChange m
  = PassChange
      ( Context.T ->
        Sexp.T ->
        NameSpace.From Symbol ->
        m (Maybe (NameSpace.From Symbol, Context.Info))
      )
  deriving (Show)

------------------------------------------------------------
-- Main Functionality
------------------------------------------------------------

-- | @passContext@ like @passContextSingle@ but we supply a different
-- function for each type term and sum representation form.
contextPassStar ::
  forall a m. (Sexp.Serialize a, HasSearch m) => Context.T -> Pass m a -> m Context.T
contextPassStar ctx pass =
  Context.traverseContextSexp ctx (sexpFunctionToPass pass)

-- | @onExpression@ runs an algorithm similar to @passContext@ however
-- only on a single expression instead of an entire context. For this
-- to have the closure work properly, please run this after :open-in is
-- gone
onExpression ::
  forall a f. (Sexp.Serialize a, HasClosure f) => Sexp.T -> PassNoCtx f a -> f Sexp.T
onExpression form func =
  Sexp.withSerialization form (`Sexp.traverseOnAtoms` func)

------------------------------------------------------------
-- Pass infrastructure
------------------------------------------------------------

sexpFunctionToPass ::
  forall a m.
  (Sexp.Serialize a, HasSearch m) =>
  Pass m a ->
  (Context.Input Sexp.T -> m (Context.Additional Sexp.T))
sexpFunctionToPass func = newFunc
  where
    newFunc (Context.Input sexp _name ctx) = do
      sexp <-
        Sexp.withSerialization
          sexp
          (`Sexp.traverseOnAtoms` (func ctx))
      pure (Context.Additional sexp [])

------------------------------------------------------------
-- Binding form infrastructure
------------------------------------------------------------

-- | @namedForms@ a list of all named special forms
namedForms :: [NameSymbol.T]
namedForms =
  [ "type",
    ":open-in",
    ":let-type",
    ":let-match",
    "case",
    ":lambda-case",
    ":declaim",
    ":lambda",
    ":tuple",
    ":primitive",
    ":progn",
    "declare",
    ":infix",
    ":list",
    ":record-no-pun",
    ":paren",
    ":block",
    ":primitive",
    ":defeff",
    ":defhandler",
    ":defop",
    ":opsig",
    ":via"
  ]

handleAtom ::
  (HasClosure m, ErrS m, Sexp.Serialize a) =>
  -- | the function for the user transformation
  -- | The Context, an extra function that is required the by the
  -- :open-in case.
  Context.T ->
  -- | The sexp form in which the atom is called on
  (BindAtom a) ->
  -- | the continuation of continuing the changes
  (BindSexp a -> m (BindSexp a)) ->
  m (BindAtom a)
handleAtom ctx (Sexp.P bind line) cont =
  Sexp.P <$> (handleBinder ctx bind cont) <*> pure line
handleAtom _ xs _ = pure xs

handleAtomNoCtx ::
  (HasClosure m) =>
  -- | The sexp form in which the atom is called on
  (BindAtom a) ->
  -- | the continuation of continuing the changes
  (BindSexp a -> m (BindSexp a)) ->
  m (BindAtom a)
handleAtomNoCtx (Sexp.P bind line) cont =
  Sexp.P <$> (handleBinderNoCtx bind cont) <*> pure line
handleAtomNoCtx xs _ =
  pure xs

handleBinder ::
  (HasClosure m, ErrS m, Sexp.Serialize a) =>
  -- | the function for the user transformation
  -- | The Context, an extra function that is required the by the
  -- :open-in case.
  Context.T ->
  -- | The sexp form in which the atom is called on
  (Bind.BinderPlus a) ->
  -- | the continuation of continuing the changes
  (Sexp.B (Bind.BinderPlus a) -> m (Sexp.B (Bind.BinderPlus a))) ->
  m (Bind.BinderPlus a)
handleBinder ctx binder cont =
  case binder of
    Bind.OpenIn arg body -> openIn ctx arg body cont
    _ -> handleBinderNoCtx binder cont

handleBinderNoCtx ::
  (HasClosure m) =>
  -- | The sexp form in which the atom is called on
  (Bind.BinderPlus a) ->
  -- | the continuation of continuing the changes
  (Sexp.B (Bind.BinderPlus a) -> m (Sexp.B (Bind.BinderPlus a))) ->
  m (Bind.BinderPlus a)
handleBinderNoCtx binder cont =
  case binder of
    Bind.Other userWanted -> pure (Bind.Other userWanted)
    Bind.OpenIn name body -> pure (Bind.OpenIn name body)
    --
    Bind.Primitive prim -> primitive prim cont
    Bind.Lambda arg body -> lambda arg body cont
    Bind.Declaim claim body -> declaim claim body cont
    Bind.LetMatch name args body -> letMatch name args body cont
    Bind.Case (Bind.Case' on body) -> case' on body cont
    Bind.LetType nameSig args body rest -> letType nameSig args body rest cont
    Bind.Type (Bind.Type' name args body) -> type' name args body cont
    Bind.LambdaCase (Bind.LambdaCase' args) -> lambdaCase args cont
    -- TODO ∷ Make work
    Bind.LetHandler form -> pure (Bind.LetHandler form)

------------------------------------------------------------
-- Environment functionality
------------------------------------------------------------

extractInformation info = Context.precedenceOf

lookupPrecedence ::
  (ErrS m, HasClosure m) => NameSymbol.T -> Context.T -> m Context.Precedence
lookupPrecedence name ctx = do
  closure <- ask @"closure"
  let symbolName = NameSymbol.hd name
  case Closure.lookup symbolName closure of
    Just info
      | NameSymbol.toSymbol name == symbolName ->
        pure $ fromMaybe Context.default' (Closure.precedenceof info)
    Just Closure.Info {mOpen = Just prefix} ->
      contextCase (prefix <> name) |> pure
    Just Closure.Info {} ->
      throwSexp (UnknownSymbol name)
    Nothing ->
      contextCase name |> pure
  where
    contextCase :: NameSymbol.T -> Context.Precedence
    contextCase name =
      (Context.lookup name ctx >>= Context.precedenceOf . (^. Context.term))
        |> fromMaybe Context.default'

------------------------------------------------------------
-- binderDispatchWith function dispatch table
------------------------------------------------------------

primitive ::
  (HasClosure m) => BindSexp a -> (BindSexp a -> m (BindSexp a)) -> m (Bind.BinderPlus a)
primitive name cont
  | Just Sexp.A {atomName} <- Sexp.atomFromT name = do
    -- this is bad, however it's FINE, as we only have the primitive
    -- which no operation should disrupt.
    -- Namely if it's (:primitive Michelson.Foo)
    -- we add Michelson to the closure, which is wrong.
    -- However, since we just have the primitive at this point, we
    -- can't have any other symbol named Michelson in this scope, so
    -- it's not an actual issue.
    local @"closure" (Closure.insertGeneric (NameSymbol.hd atomName)) $ do
      Bind.Primitive <$> cont name
primitive _ _ = error "malformed primitive"

lambda ::
  (HasClosure m) =>
  BindSexp a ->
  BindSexp a ->
  (BindSexp a -> m (BindSexp a)) ->
  m (Bind.BinderPlus a)
lambda args body cont =
  local @"closure" (\c -> foldr Closure.insertGeneric c (nameStar args)) $
    Bind.Lambda <$> cont args <*> cont body

letType ::
  (HasClosure m) =>
  BindSexp a ->
  BindSexp a ->
  BindSexp a ->
  BindSexp a ->
  (BindSexp a -> m (BindSexp a)) ->
  m (Bind.BinderPlus a)
letType nameAndSig args body rest cont = do
  let bindings = nameStar args
      consturctors = nameGather body
      closureUpdate cnt =
        foldr Closure.insertGeneric cnt (bindings <> consturctors)
  local @"closure" closureUpdate $
    Bind.LetType <$> cont nameAndSig <*> pure args <*> cont body <*> cont rest

type' ::
  (HasClosure m) =>
  BindSexp a ->
  BindSexp a ->
  BindSexp a ->
  (BindSexp a -> m (BindSexp a)) ->
  m (Bind.BinderPlus a)
type' nameAndSig args body cont =
  let grabBindings = nameStar args
      nameOfConstr = nameOf nameAndSig
   in local @"closure" (\cnt -> foldr Closure.insertGeneric cnt (nameOfConstr <> grabBindings)) $
        Bind.Type' <$> cont nameAndSig <*> pure args <*> cont body
          >>| Bind.Type

nameOf :: Sexp.B a -> [Symbol]
nameOf sexp =
  case Sexp.car (Sexp.car sexp) of
    Sexp.Atom (Sexp.A {atomName}) -> [NameSymbol.toSymbol atomName]
    _____________________________ -> []

-- | @openIn@ opens @mod@, adding the contents to the closure of
-- @body@. Note that we first =resolve= what mod is by calling the
-- continuation, @cont@, in case any transformations want to change
-- what the @mod@ is.
openIn ::
  (ErrS m, HasClosure m, Sexp.Serialize a) =>
  Context.T ->
  BindSexp a ->
  BindSexp a ->
  (BindSexp a -> m (BindSexp a)) ->
  m (Bind.BinderPlus a)
openIn ctx name body cont = do
  -- Fully run what we need to on mod
  newMod <- cont name
  -- Now let us open up the box
  case Sexp.atomFromT newMod of
    Just Sexp.A {atomName} ->
      case ctx Context.!? atomName >>| (^. Context.def) . Context.extractValue of
        Just (Context.Module record) ->
          let NameSpace.List {publicL} = NameSpace.toList (record ^. Context.contents)
              --
              newSymbs = Juvix.Library.fst <$> publicL
              --
              addSymbolInfo symbol =
                Closure.insert symbol (Closure.Info Nothing [] (Just atomName))
           in --
              local @"closure" (\cnt -> foldr addSymbolInfo cnt newSymbs) $
                Bind.OpenIn newMod <$> cont body
        _ ->
          throwSexp (CantResolve [Sexp.serialize newMod])
    _ ->
      throwSexp (CantResolve [Sexp.serialize newMod])

-- | @lambdaCase@ we encounter a @:lambda-case@ at the start of every
-- Definition in the context. This ensures the arguments are properly
-- bound for the inner computation.
lambdaCase ::
  (HasClosure f) =>
  [Bind.DeconBody (Bind.BinderPlus a)] ->
  (BindSexp a -> f (BindSexp a)) ->
  f (Bind.BinderPlus a)
lambdaCase args cont =
  Bind.LambdaCase . Bind.LambdaCase' <$> traverse (`matchMany` cont) args

letMatch ::
  (HasClosure m) =>
  Symbol ->
  [Bind.ArgBody (Bind.BinderPlus a)] ->
  BindSexp a ->
  (BindSexp a -> m (BindSexp a)) ->
  m (Bind.BinderPlus a)
letMatch name args body cont =
  local @"closure" (Closure.insertGeneric name) $
    Bind.LetMatch name
      <$> traverse (fmap Bind.deconToArg . (`matchMany` cont) . Bind.argToDecon) args
      <*> cont body

case' ::
  (HasClosure f) =>
  BindSexp a ->
  [Bind.DeconBody (Bind.BinderPlus a)] ->
  (BindSexp a -> f (BindSexp a)) ->
  f (Bind.BinderPlus a)
case' on bodies cont =
  Bind.Case' <$> cont on <*> traverse (`match` cont) bodies
    >>| Bind.Case

-- This works as we should only do a declaration after the function
-- locally, so if it gets overwritten its' not a big deal

-- | @declaim@ takes a declaration and adds the declaration information
-- to the context
declaim ::
  (HasClosure f) =>
  BindSexp a ->
  BindSexp a ->
  (BindSexp a -> f (BindSexp a)) ->
  f (Bind.BinderPlus a)
declaim claim body cont
  | Just (name, information) <- declaration claim =
    local @"closure" (Closure.insert name information) $
      -- safe to do dec here, as if we modify the declaration it
      -- would be fine to do it after, as all a pass would do is to
      -- make it a namesymbol, meaning it wouldn't work as is ☹
      Bind.Declaim <$> cont claim <*> cont body
declaim _ _ _ = error "malformed declaim"

------------------------------------------------------------
-- Helpers for the various Search and Closure dispatch
------------------------------------------------------------

-- | @matchMany@ deals with a @((binding-1 … binding-n) body) term, and
-- proper continues the transformation on the body, and the bindings
-- after making sure to register that they are indeed bound terms
matchMany ::
  (HasClosure m) => Bind.DeconBody a -> (Sexp.B a -> m (Sexp.B a)) -> m (Bind.DeconBody a)
matchMany = matchGen nameStar

match ::
  (HasClosure m) => Bind.DeconBody a -> (Sexp.B a -> m (Sexp.B a)) -> m (Bind.DeconBody a)
match = matchGen nameStarSingle

-- | @matchGen@ is a generic/general version of match and matchMany as
-- the form that comes in may be a list of binders or a single term
-- being bound.
matchGen ::
  (HasClosure m, Foldable t) =>
  (Sexp.B a -> t Symbol) ->
  Bind.DeconBody a ->
  (Sexp.B a -> m (Sexp.B a)) ->
  m (Bind.DeconBody a)
matchGen nameStarFunc (Bind.DeconBody args body) cont =
  -- Important we must do this first!
  local @"closure" (\cnt -> foldr Closure.insertGeneric cnt grabBindings) $
    -- THIS MUST happen in the local, as we don't want to have a pass
    -- confuse the variables here as something else... imagine if we
    -- are doing a pass which resolves symbols, then we'd try to
    -- resolve the variables we bind. However for constructors and what
    -- not they need to be ran through this pass
    Bind.DeconBody <$> cont args <*> cont body
  where
    grabBindings = nameStarFunc args

-- | @nameStarSingle@ like @nameStar@ but we are matching on a single
-- element
nameStarSingle :: Sexp.B a -> [Symbol]
nameStarSingle = nameStar . (\x -> Sexp.list [x])

-- | @nameStar@ grabs names recursively
nameStar :: Sexp.B a -> [Symbol]
nameStar ((_caar Sexp.:> cadr) Sexp.:> cdr) =
  -- we ignore the _caar as it's a cosntructor!
  nameStar cadr <> nameStar cdr
nameStar (x Sexp.:> xs)
  | Just symb <- eleToSymbol x =
    symb : nameStar xs
  | otherwise =
    -- the car is not a cons or an atom, thus a number, we should
    -- ignore it
    nameStar xs
nameStar Sexp.Atom {} = []
nameStar Sexp.Nil = []

-- Sexp.parse "((Cons (:arrow (:infix -> Int Int))) (Nil))" >>| nameGather

-- | @nameGather@ takes an adt sexp and extracts the constructors from it
nameGather :: Sexp.B a -> [Symbol]
nameGather ((caar Sexp.:> _cdar) Sexp.:> cdr)
  | Just symb <- eleToSymbol caar,
    symb /= ":" || symb /= ":record-d" =
    symb : nameGather cdr
nameGather (_ Sexp.:> cdr) = nameGather cdr
nameGather _ = []

------------------------------------------------------------
-- Helpers for declaim
------------------------------------------------------------

-- | @declaration@ takes a declaration and tries to get the information
-- along with the name from it.
-- - Note :: we can only get symbol declarations to update, as we rely
--   on closure semantics whicich only work on symbols unfortunately.
declaration :: Sexp.B a -> Maybe (Symbol, Closure.Information)
declaration (Sexp.List [inf, n, i])
  | Just Sexp.N {atomNum} <- Sexp.atomFromT i,
    Just atomName <- eleToSymbol n =
    let func =
          if
              | Sexp.isAtomNamed inf "infix" ->
                Context.NonAssoc
              | Sexp.isAtomNamed inf "infixl" ->
                Context.Left
              | Sexp.isAtomNamed inf "infixr" ->
                Context.Right
              | otherwise -> error "malformed declaration"
     in Just
          ( atomName,
            Closure.Info
              Nothing
              [Closure.Prec $ Context.Pred func (fromInteger atomNum)]
              Nothing
          )
declaration _ = Nothing

------------------------------------------------------------
-- Move to Sexp library
------------------------------------------------------------

eleToSymbol :: Sexp.B a -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing
