{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Erased.Base.Types
  ( module Juvix.Core.Erased.Base.Types,
    Core.Universe,
    pattern Core.VF, Core.vfName, Core.vfVal,
    pattern Core.TF, Core.tfUsage, Core.tfName, Core.tfType,
  )
where

import Extensible
import qualified Juvix.Core.Base.Types as Core
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

extensible
  [d|
    data Term primVal
      = Var NameSymbol.T
      | Prim primVal
      | -- TODO ∷ add proper lam with capture and arguments here!
        Lam NameSymbol.T (Term primVal)
      | Pair (Term primVal) (Term primVal)
      | CatProductIntro (Term primVal) (Term primVal)
      | CatProductElimLeft (Term primVal) (Term primVal)
      | CatProductElimRight (Term primVal) (Term primVal)
      | CatCoproductIntroLeft (Term primVal)
      | CatCoproductIntroRight (Term primVal)
      | CatCoproductElim (Term primVal) (Term primVal) (Term primVal) (Term primVal) (Term primVal)
      | Unit
      | Record [ValField primVal]
      | Let NameSymbol.T (Term primVal) (Term primVal)
      | App (Term primVal) (Term primVal)
      | RecElim [Symbol] (Term primVal) (Term primVal)
      deriving (Show, Eq, Generic)

    type ValField primVal = Core.ValField' (Term primVal)

    data Type primTy
      = SymT NameSymbol.T
      | Star Core.Universe
      | PrimTy primTy
      | -- TODO: How to deal with dependency?
        Pi Usage.T (Type primTy) (Type primTy)
      | UnitTy
      | Sig Usage.T (Type primTy) (Type primTy)
      | CatProduct (Type primTy) (Type primTy)
      | CatCoproduct (Type primTy) (Type primTy)
      | RecordTy [TypeField primTy]
      deriving (Show, Eq, Generic)

    type TypeField primTy = Core.TypeField' (Type primTy)

    type TypeAssignment primTy = Map.T NameSymbol.T (Type primTy)
    |]
