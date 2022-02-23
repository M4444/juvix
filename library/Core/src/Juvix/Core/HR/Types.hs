module Juvix.Core.HR.Types
  ( module Juvix.Core.HR.Types,
  )
where

import Juvix.Core.Application (IsParamVar (..))
import qualified Juvix.Core.Base.Types as Core
import Juvix.Core.HR.Extend
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

data T deriving (Data, Show)

instance IsParamVar T where
  type ParamVar T = NameSymbol.T
  freeVar _ = Just
  boundVar _ _ = Nothing

Core.extendTerm "Term" [] [t|T|] extTerm

type TypeField primTy primVal = Core.TypeField T primTy primVal
type ValueField primTy primVal = Core.ValField T primTy primVal

-- TODO allow extendTerm to reorder fields?
pattern Lam x t = Lam0 t x

pattern Pi π x s t = Pi0 π s t x

pattern Sig π x s t = Sig0 π s t x

pattern Let π x l b = Let0 π l b x

{-# COMPLETE Star, PrimTy, Prim, Pi, Lam, Sig, Pair,
             CatProduct, CatCoproduct,
             CatProductIntro, CatProductElimLeft, CatProductElimRight,
             CatCoproductIntroLeft, CatCoproductIntroRight, CatCoproductElim,
             RecordTy, Record,
             UnitTy, Unit, Let, Elim #-}

Core.extendElim "Elim" [] [t|T|] extElim

pattern RecElim ns e x a t = RecElim0 ns e a t x

{-# COMPLETE Var, App, RecElim, Ann #-}

Core.extendPattern "Pattern" [] [t|T|] extPattern
