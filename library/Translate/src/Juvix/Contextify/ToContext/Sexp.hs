{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Juvix.Contextify.ToContext.Sexp
  ( run,
    contextify,
  )
where

import Control.Lens (over, set, (^.))
import qualified Juvix.Context as Context
import qualified Juvix.Context.InfoHelper as Info
import qualified Juvix.Context.InfoNames as Info
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Contextify.ToContext.Types as Type
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import Juvix.Sexp.Structure.Lens
import qualified Juvix.Sexp.Structure.Transition as Structure
import Prelude (error)

-- the name symbols are the modules we are opening
-- TODO ∷ parallelize this
run,
  contextify ::
    MonadIO m =>
    Context.T ->
    (Context.NameSymbol, [Sexp.T]) ->
    m (Either Context.PathError Type.PassSexp)
contextify cont (nameSymb, xs) = do
  newNamespace <- liftIO $ Context.switchNameSpace nameSymb cont
  case newNamespace of
    Left errr -> pure (Left errr)
    Right ctxS ->
      foldM f Type.PS {ctxS, opensS = [], modsDefinedS = []} xs
        |> liftIO
        >>| Right
  where
    f Type.PS {ctxS, opensS, modsDefinedS} top = do
      Type.PS {ctxS = ctx', opensS = opens', modsDefinedS = modsDefined'} <-
        updateTopLevel top ctxS
      pure
        Type.PS
          { ctxS = ctx',
            opensS = opensS <> opens',
            modsDefinedS = modsDefinedS <> modsDefined'
          }
run = contextify

updateTopLevel :: Sexp.T -> Context.T -> IO Type.PassSexp
updateTopLevel x ctx
  | Sexp.isAtomNamed x ":type-class" = pure $ Type.PS ctx [] []
  | Sexp.isAtomNamed x ":instance" = pure $ Type.PS ctx [] []
updateTopLevel full@(name Sexp.:> body) ctx
  | named ":defsig-match" = defun full ctx
  | named "declare" = declare body ctx
  | named "type" = type' body ctx
  | named "open" = open body ctx
  where
    named = Sexp.isAtomNamed name
updateTopLevel _ ctx = pure $ Type.PS ctx [] []

-- | @defun@ takes a defun form and shoves it in a Definition in the
-- top levevl context. Since modules aren't identified yet, we first
-- check to see if they are a module, and if they are a very simple
-- module, then we insert a Record into the Context, and not a definition
defun :: Sexp.T -> Context.T -> IO Type.PassSexp
defun (Structure.toDefunSigMatch -> Just defn) ctx
  | Just name <- eleToSymbol (defn ^. name) = do
    let ctxWithSigInfo =
          case defn ^. sig of
            Sexp.Nil -> ctx
            -- TODO ∷ add "type" to a proper name not just here
            signatur -> Info.injectMetaInformation ctx name (Info.signature, signatur)
        term =
          Structure.LambdaCase (defn ^. args) |> Structure.fromLambdaCase
    pure $ Type.PS (Info.injectNewTerm ctxWithSigInfo name term) [] []
defun _ _ctx = error "malformed defun"

-- | @declare@ takes a declaration and tries to add it to the
-- context. This is a bit tricky, as we could have seen the definition
-- before hand... that part isn't guaranteed, so if we find it then
-- great! Add it to a definition, however if we can't find it, then
-- make a note about the information we have on this symbol, the
-- definition will handle incorporating it
declare :: Sexp.T -> Context.T -> IO Type.PassSexp
declare (Sexp.List [inf, n, i]) ctx
  | Just Sexp.N {atomNum} <- Sexp.atomFromT i,
    Just atomName <- eleToSymbol n =
    let prec =
          Context.Pred
            if
                | Sexp.isAtomNamed inf "infix" ->
                  Context.NonAssoc
                | Sexp.isAtomNamed inf "infixl" ->
                  Context.Left
                | Sexp.isAtomNamed inf "infixr" ->
                  Context.Right
                | otherwise -> error "malformed declaration"
            (fromIntegral atomNum)
        newCtx =
          Info.injectMetaInformation ctx atomName (Info.precedence, Sexp.serialize prec)
     in Type.PS newCtx [] [] |> pure
declare _ _ = error "malformed declare"

-- | @type'@ will take its type and add it into the context. Note that
-- since we store information with the type, we will keep the name in
-- the top level form.
type' :: Sexp.T -> Context.T -> IO Type.PassSexp
type' t@(assocName Sexp.:> _ Sexp.:> dat) ctx
  | Just name <- eleToSymbol (Sexp.car assocName) =
    let constructors = collectConstructors dat
        addSum con ctx =
          Structure.SumCon (pure name)
            |> Structure.fromSumCon
            |> Info.injectNewTerm ctx con
        newCtx = foldr addSum ctx constructors
        ctxWithType =
          Info.injectNewTerm newCtx name (Sexp.Cons (Sexp.atom "type") t)
     in Type.PS ctxWithType [] [] |> pure
type' _ _ = error "malformed type"

-- | @open@ like type will simply take the open and register that the
-- current module is opening it. Since the context does not have such a
-- notion, we have to store this information for the resolve module to
-- properly handle
open :: Sexp.T -> Context.T -> IO Type.PassSexp
open (Sexp.List [mod]) ctx
  | Just Sexp.A {atomName} <- Sexp.atomFromT mod =
    pure $
      Type.PS
        { ctxS = ctx,
          opensS = [atomName],
          modsDefinedS = []
        }
open _ _ = error "malformed open"

collectConstructors :: Sexp.T -> [Symbol]
collectConstructors dat
  | Just s <- Sexp.toList dat =
    case traverse (eleToSymbol . Sexp.car) s of
      Nothing -> []
      Just xs -> xs
  -- filter out the record constructors, which really aren't constructors
  -- filter (\x -> x /= ":record-d" && x /= ":") xs
  | otherwise = []

--------------------------------------------------------------------------------
-- General Helpers
--------------------------------------------------------------------------------
eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing
