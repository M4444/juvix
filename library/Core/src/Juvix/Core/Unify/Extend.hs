module Juvix.Core.Unify.Extend (extTerm, extElim, extValue, extNeutral) where

import Extensible
import Juvix.Core.Base.Types.Base
import Juvix.Core.Unify.MetaVar

extTerm :: TypeQ -> TypeQ -> ExtTerm
extTerm _ty _val =
  defaultExtTerm
    { typeTermX = [("Meta", [[t|MetaVar|]])]
    }

extElim :: TypeQ -> TypeQ -> ExtElim
extElim _ty _val = defaultExtElim

extValue :: TypeQ -> TypeQ -> ExtValue
extValue _ty _val =
  defaultExtValue
    { typeValueX = [("VMeta", [[t|MetaVar|]])]
    }

extNeutral :: TypeQ -> TypeQ -> ExtNeutral
extNeutral _ty _val = defaultExtNeutral
