{-# LANGUAGE DeriveFunctor #-}

-- | Traversals serves as generic Context Traversal modules.
module Juvix.Context.Traversal where

import Control.Lens hiding ((|>))
import qualified Juvix.Context as Context
import qualified Juvix.Context.NameSpace as NameSpace
import Juvix.Context.Types
import Juvix.Library hiding (Sum, modify, toList)
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

------------------------------------------------------------
-- Traversal Function Transformers
------------------------------------------------------------
data Additional dat = Additional
  { formPutBack :: dat,
    extraPutBack :: [(NameSpace.From Symbol, Instruction)]
  }
  deriving (Show, Functor)

-- | @Instruction@ denotes what we should do with a given term
data Instruction
  = Add Info
  | Delete
  deriving (Show)

data Input dat = Input
  { info :: dat,
    name :: NameSpace.From Symbol,
    currentContext :: T
  }
  deriving (Show)

------------------------------------------------------------
-- Global Traversals
------------------------------------------------------------

mapContext :: T -> (Input Info -> Identity (Additional Info)) -> T
mapContext t f = runIdentity (traverseContext t f)

traverseContextSexp ::
  Monad m => T -> (Input Sexp.T -> m (Additional Sexp.T)) -> m T
traverseContextSexp t = traverseContext t . overInput

-- | @overInput@ allows a function to run on the sexps alone of the
-- context. Note that the record is skipped over in such cases.
overInput ::
  Monad m => (Input Sexp.T -> m (Additional Sexp.T)) -> (Input Info -> m (Additional Info))
overInput sexpF = infoF
  where
    infoF (Input {info = (Info table def), name, currentContext}) = do
      Additional term extra <-
        case def of
          Context.Term sexp -> do
            sexpAdd <- sexpF (defaultInput sexp)
            pure (fmap Context.Term sexpAdd)
          _ ->
            pure (Additional def [])
      (extra, newTable) <- traverseAccumM combineF extra table
      pure $ Additional (Info newTable term) extra
      where
        defaultInput sexp =
          (Input {info = sexp, name, currentContext})
        combineF putBack sexp = do
          Additional sexp extra <- sexpF (defaultInput sexp)
          pure (putBack <> extra, sexp)

traverseContext :: Monad m => T -> (Input Info -> m (Additional Info)) -> m T
traverseContext t f = do
  updatedEntireContext <- foldM siwtchTopAndUpdate t tops
  case Context.inNameSpace (Context.addTopName (t ^. _currentName)) updatedEntireContext of
    Nothing -> pure updatedEntireContext
    Just ct -> pure ct
  where
    tops =
      HashMap.keys (t ^. _topLevelMap)
        >>| NameSymbol.fromSymbol
        >>| Context.addTopName
    siwtchTopAndUpdate ctx topLevelSym =
      case Context.inNameSpace topLevelSym ctx of
        Nothing -> pure ctx
        Just ns -> traverseCurrentContext ns f

traverseCurrentContext :: Monad m => T -> (Input Info -> m (Additional Info)) -> m T
traverseCurrentContext t f = do
  ctxOnCurrent <- foldM applyFunctionOnLocalTerms t (grabCurrentNameSpace t)
  --
  let inputListNew = grabCurrentNameSpace ctxOnCurrent |> fmap fst
  --
  foldM recursivelySwitchAndApply ctxOnCurrent inputListNew
  where
    applyFunctionOnLocalTerms ctx (sym, info) = do
      Additional back insts <- f (Input {info, name = sym, currentContext = ctx})
      let newCtx = runInsts (Context.add sym back ctx) insts
      pure newCtx
    --
    recursivelySwitchAndApply ctx sym =
      case Context.inNameSpace (NameSymbol.fromSymbol (NameSpace.extractValue sym)) ctx of
        Just tUpdatedCurrentModule -> do
          --
          newCtx <- traverseCurrentContext tUpdatedCurrentModule f
          --
          let Just back =
                Context.inNameSpace (Context.addTopName (t ^. _currentName)) newCtx
          --
          case sym of
            NameSpace.Pub _sym -> pure back
            NameSpace.Priv sym -> Context.markPrivate sym back |> pure
        Nothing ->
          pure ctx

grabCurrentNameSpace :: T -> [(NameSpace.From Symbol, Info)]
grabCurrentNameSpace t =
  let current = t ^. Context._currentNameSpace . Context.record . Context.contents
   in NameSpace.toList1FSymb current

runInsts :: T -> [(NameSpace.From Symbol, Instruction)] -> T
runInsts ctx xs =
  foldl runInst ctx xs
  where
    runInst ctx (sym, Add info) = Context.add sym info ctx
    runInst ctx (sym, Delete) = Context.remove sym ctx
