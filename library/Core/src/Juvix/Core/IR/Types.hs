{-# LANGUAGE DeriveAnyClass #-}

-- | Quantitative type implementation inspired by
--   Atkey 2018 and McBride 2016.
module Juvix.Core.IR.Types
  ( module Juvix.Core.IR.Types,
  )
where

import qualified Juvix.Core.Base.Types as Core
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Juvix.Sexp.Serialize as Serialize

data T
  deriving
    ( Read,
      Show,
      Eq,
      Ord,
      Generic,
      Typeable,
      Data
    )
  deriving anyclass
    ( NFData
    )

instance Serialize.DefaultOptions T where
  defaultOptions = Serialize.Options mempty mempty

instance Serialize.Serialize T where
  serialize _ = undefined
  deserialize _ = Nothing

Core.extendTerm "Term" [] [t|T|] $ \_ _ -> Core.defaultExtTerm

Core.extendElim "Elim" [] [t|T|] $ \_ _ -> Core.defaultExtElim

Core.extendValue "Value" [] [t|T|] $ \_ _ -> Core.defaultExtValue

Core.extendNeutral "Neutral" [] [t|T|] $ \_ _ -> Core.defaultExtNeutral

Core.extendPattern "Pattern" [] [t|T|] $ \_ _ -> Core.defaultExtPattern

type EqExt ext primTy primVal =
  ( Core.TermAll Eq ext primTy primVal,
    Core.ElimAll Eq ext primTy primVal,
    Core.ValueAll Eq ext primTy primVal,
    Core.NeutralAll Eq ext primTy primVal,
    Core.PatternAll Eq ext primTy primVal,
    Eq (Core.TermX ext primTy primVal),
    Eq (Core.ElimX ext primTy primVal),
    Eq (Core.ValueX ext primTy primVal),
    Eq (Core.NeutralX ext primTy primVal),
    Eq ext,
    Eq primTy,
    Eq primVal
  )

usageToGlobal :: Usage.T -> Maybe Core.GlobalUsage
usageToGlobal Usage.SAny = Just Core.GSAny
usageToGlobal (Usage.SNat 0) = Just Core.GZero
usageToGlobal _ = Nothing

globalToUsage :: Core.GlobalUsage -> Usage.T
globalToUsage Core.GSAny = Usage.SAny
globalToUsage Core.GZero = Usage.SNat 0

globalName :: Core.Global extT extV primTy primVal -> NameSymbol.T
globalName (Core.GDatatype (Core.Datatype {dataName})) = dataName
globalName (Core.GDataCon (Core.DataCon {dataConName})) = dataConName
globalName (Core.GFunction (Core.Function {funName})) = funName
globalName (Core.GAbstract (Core.Abstract {absName})) = absName
