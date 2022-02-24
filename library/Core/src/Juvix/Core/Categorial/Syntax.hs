module Juvix.Core.Categorial.Syntax
  ( atom,
    cons,
    keyword,
  )
where

import qualified Control.Monad.Trans.Except as ExceptT
import Data.Text (unpack)
import Juvix.Core.Categorial.Errors (SyntaxError (..))
import Juvix.Core.Categorial.Private.TermPrivate
  ( MinimalInstanceAlgebra,
    Symbol (..),
    Term (..),
  )
import Juvix.Library
  ( Maybe (..),
    Monad,
    Text,
    readMaybe,
    return,
    ($),
    (.),
    (==),
  )
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Types as SexpTypes

type SyntaxResultT m a freeAlgObj = ExceptT.ExceptT (SyntaxError freeAlgObj) m a

nilText :: Text
nilText = "nil"

atom ::
  ( Monad m,
    MinimalInstanceAlgebra a
  ) =>
  a ->
  SyntaxResultT m (Term a) a
atom = return . SexpRepresentation . Sexp.primOp . Variable

keyword ::
  ( Monad m,
    MinimalInstanceAlgebra a
  ) =>
  Text ->
  SyntaxResultT m (Term a) a
keyword s | s == nilText = return $ SexpRepresentation SexpTypes.Nil
keyword s = case readMaybe (unpack s) of
  Just k -> return $ SexpRepresentation $ Sexp.primOp $ Keyword k
  Nothing -> ExceptT.throwE $ NoSuchKeyword s

cons ::
  ( Monad m,
    MinimalInstanceAlgebra a
  ) =>
  Term a ->
  Term a ->
  SyntaxResultT m (Term a) a
cons (SexpRepresentation x) (SexpRepresentation x') =
  return $ SexpRepresentation $ Sexp.Cons x x'
cons (RepresentedTerm t) _ = ExceptT.throwE $ ConsOfRepresentedTerm t
cons _ (RepresentedTerm t) = ExceptT.throwE $ ConsOfRepresentedTerm t
