{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Core.Unify.Term where

import qualified Juvix.Core.Base.TransformExt as TE
import qualified Juvix.Core.Base.Types.Base as Core
import qualified Juvix.Core.IR.Types as IR
import Juvix.Core.Unify.Extend
import Juvix.Core.Unify.MetaVar (MetaSet)
import qualified Juvix.Core.Unify.MetaVar as Meta
import Juvix.Library

data T

data UnsolvedMetas a = Unsolved MetaSet | Solved a deriving (Functor)

instance Applicative UnsolvedMetas where
  pure = Solved
  Unsolved xs <*> Unsolved ys = Unsolved $ xs <> ys
  Unsolved xs <*> Solved _ = Unsolved xs
  Solved _ <*> Unsolved ys = Unsolved ys
  Solved f <*> Solved x = Solved $ f x

Core.extendTerm "Term" [] [t|T|] extTerm

Core.extendElim "Elim" [] [t|T|] extElim

injectorTE :: TE.ExtTransformTE IR.T T primTy primVal
injectorTE =
  TE.ExtTransformTE
    { etStar = identity,
      etPrimTy = identity,
      etPrim = identity,
      etPi = identity,
      etSig = identity,
      etPair = identity,
      etCatProduct = identity,
      etCatCoproduct = identity,
      etCatProductIntro = identity,
      etCatProductElimLeft = identity,
      etCatProductElimRight = identity,
      etCatCoproductIntroLeft = identity,
      etCatCoproductIntroRight = identity,
      etCatCoproductElim = identity,
      etUnitTy = identity,
      etUnit = identity,
      etLam = identity,
      etLet = identity,
      etElim = identity,
      etBound = identity,
      etFree = identity,
      etApp = identity,
      etAnn = identity,
      etTermX = absurd,
      etElimX = identity
    }

inTerm :: IR.Term primTy primVal -> Term primTy primVal
inTerm = TE.extTransformT injectorTE

inElim :: IR.Elim primTy primVal -> Elim primTy primVal
inElim = TE.extTransformE injectorTE

ejectorTE :: TE.ExtTransformTEF UnsolvedMetas T IR.T primTy primVal
ejectorTE =
  TE.ExtTransformTEF
    { etfStar = pure,
      etfPrimTy = pure,
      etfPrim = pure,
      etfPi = pure,
      etfLam = pure,
      etfSig = pure,
      etfUnitTy = pure,
      etfUnit = pure,
      etfPair = pure,
      etfCatProduct = pure,
      etfCatCoproduct = pure,
      etfCatProductIntro = pure,
      etfCatProductElimLeft = pure,
      etfCatProductElimRight = pure,
      etfCatCoproductIntroLeft = pure,
      etfCatCoproductIntroRight = pure,
      etfCatCoproductElim = pure,
      etfLet = pure,
      etfElim = pure,
      etfBound = pure,
      etfFree = pure,
      etfApp = pure,
      etfAnn = pure,
      etfTermX = Unsolved . Meta.singleS,
      etfElimX = pure
    }

outTerm :: Term primTy primVal -> UnsolvedMetas (IR.Term primTy primVal)
outTerm = TE.extTransformTF ejectorTE

outElim :: Elim primTy primVal -> UnsolvedMetas (IR.Elim primTy primVal)
outElim = TE.extTransformEF ejectorTE

Core.extendValue "Value" [] [t|T|] extValue

Core.extendNeutral "Neutral" [] [t|T|] extNeutral

injectorVN :: TE.ExtTransformVN IR.T T primTy primVal
injectorVN =
  TE.ExtTransformVN
    { etVStar = identity,
      etVPrimTy = identity,
      etVPrim = identity,
      etVPi = identity,
      etVSig = identity,
      etVPair = identity,
      etVCatProduct = identity,
      etVCatCoproduct = identity,
      etVCatProductIntro = identity,
      etVCatProductElimLeft = identity,
      etVCatProductElimRight = identity,
      etVCatCoproductIntroLeft = identity,
      etVCatCoproductIntroRight = identity,
      etVCatCoproductElim = identity,
      etVUnitTy = identity,
      etVUnit = identity,
      etVLam = identity,
      etVNeutral = identity,
      etNBound = identity,
      etNFree = identity,
      etNApp = identity,
      etValueX = absurd,
      etNeutralX = identity
    }

inValue :: IR.Value primTy primVal -> Value primTy primVal
inValue = TE.extTransformV injectorVN

inNeutral :: IR.Neutral primTy primVal -> Neutral primTy primVal
inNeutral = TE.extTransformN injectorVN

ejectorVN :: TE.ExtTransformVNF UnsolvedMetas T IR.T primTy primVal
ejectorVN =
  TE.ExtTransformVNF
    { etfVStar = pure,
      etfVPrimTy = pure,
      etfVPrim = pure,
      etfVPi = pure,
      etfVLam = pure,
      etfVSig = pure,
      etfVUnitTy = pure,
      etfVUnit = pure,
      etfVPair = pure,
      etfVCatProduct = pure,
      etfVCatCoproduct = pure,
      etfVCatProductIntro = pure,
      etfVCatProductElimLeft = pure,
      etfVCatProductElimRight = pure,
      etfVCatCoproductIntroLeft = pure,
      etfVCatCoproductIntroRight = pure,
      etfVCatCoproductElim = pure,
      etfVNeutral = pure,
      etfNBound = pure,
      etfNFree = pure,
      etfNApp = pure,
      etfValueX = Unsolved . Meta.singleS,
      etfNeutralX = pure
    }

outValue :: Value primTy primVal -> UnsolvedMetas (IR.Value primTy primVal)
outValue = TE.extTransformVF ejectorVN

outNeutral :: Neutral primTy primVal -> UnsolvedMetas (IR.Neutral primTy primVal)
outNeutral = TE.extTransformNF ejectorVN
