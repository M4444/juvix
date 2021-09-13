module Juvix.Witch.CPSTranslation where

import Juvix.Witch.CPSTranslation.Transform
import Juvix.Library
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Context as Context

op :: (ExpressionIO m)
   => Context.T Sexp.T ty sumRep
   -> [Sexp.T]
   -> ContextifiedProgram m ty sumRep
op context sexp = do
  (context', sexp') <- addHandlersToContext context sexp
  vias <- pure (collectVia sexp')
  convertVias (mkContextProg context' vias)
