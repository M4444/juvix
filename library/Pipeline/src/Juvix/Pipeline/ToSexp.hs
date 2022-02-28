{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.Pipeline.ToSexp
  ( Error (..),
    module ToSexp,
  )
where

import qualified Juvix.Context as Context
import qualified Juvix.Contextify as Contextify
import Juvix.Core.Erased.Algorithm (erase, eraseAnn)
import qualified Juvix.Core.HR.Pretty as HR
import Juvix.Core.Translate
import Juvix.Core.Types
import qualified Juvix.Desugar as Desugar
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Parsing.Types as Initial
import qualified Juvix.Sexp as Sexp
import Juvix.Translate.Pipeline as ToSexp

-- | Frontend Error
data Error
  = ContextErr Contextify.ResolveErr
  | DesugarErr
  deriving (Show)

type instance PP.Ann Error = ()

instance PP.PrettyText Error where
  prettyT = \case
    ContextErr err -> PP.show err -- FIXME
    DesugarErr -> PP.text "no input after desugaring"
