{-# LANGUAGE ViewPatterns #-}

-- | Transformations between different extensions.
module Juvix.Core.Base.TransformExt where

import Data.Coerce
import Juvix.Core.Base.Types
import Juvix.Library hiding (Coerce)

data ExtTransformTEF f ext1 ext2 primTy primVal = ExtTransformTEF
  { etfStar :: XStar ext1 primTy primVal -> f (XStar ext2 primTy primVal),
    etfPrimTy :: XPrimTy ext1 primTy primVal -> f (XPrimTy ext2 primTy primVal),
    etfPrim :: XPrim ext1 primTy primVal -> f (XPrim ext2 primTy primVal),
    etfPi :: XPi ext1 primTy primVal -> f (XPi ext2 primTy primVal),
    etfLam :: XLam ext1 primTy primVal -> f (XLam ext2 primTy primVal),
    etfSig :: XSig ext1 primTy primVal -> f (XSig ext2 primTy primVal),
    etfPair :: XPair ext1 primTy primVal -> f (XPair ext2 primTy primVal),
    etfCatProduct :: XCatProduct ext1 primTy primVal -> f (XCatProduct ext2 primTy primVal),
    etfCatCoproduct :: XCatCoproduct ext1 primTy primVal -> f (XCatCoproduct ext2 primTy primVal),
    etfCatProductIntro :: XCatProductIntro ext1 primTy primVal -> f (XCatProductIntro ext2 primTy primVal),
    etfCatProductElimLeft :: XCatProductElimLeft ext1 primTy primVal -> f (XCatProductElimLeft ext2 primTy primVal),
    etfCatProductElimRight :: XCatProductElimRight ext1 primTy primVal -> f (XCatProductElimRight ext2 primTy primVal),
    etfCatCoproductIntroLeft :: XCatCoproductIntroLeft ext1 primTy primVal -> f (XCatCoproductIntroLeft ext2 primTy primVal),
    etfCatCoproductIntroRight :: XCatCoproductIntroRight ext1 primTy primVal -> f (XCatCoproductIntroRight ext2 primTy primVal),
    etfCatCoproductElim :: XCatCoproductElim ext1 primTy primVal -> f (XCatCoproductElim ext2 primTy primVal),
    etfUnitTy :: XUnitTy ext1 primTy primVal -> f (XUnitTy ext2 primTy primVal),
    etfUnit :: XUnit ext1 primTy primVal -> f (XUnit ext2 primTy primVal),
    etfLet :: XLet ext1 primTy primVal -> f (XLet ext2 primTy primVal),
    etfElim :: XElim ext1 primTy primVal -> f (XElim ext2 primTy primVal),
    etfBound :: XBound ext1 primTy primVal -> f (XBound ext2 primTy primVal),
    etfFree :: XFree ext1 primTy primVal -> f (XFree ext2 primTy primVal),
    etfApp :: XApp ext1 primTy primVal -> f (XApp ext2 primTy primVal),
    etfAnn :: XAnn ext1 primTy primVal -> f (XAnn ext2 primTy primVal),
    etfTermX :: TermX ext1 primTy primVal -> f (TermX ext2 primTy primVal),
    etfElimX :: ElimX ext1 primTy primVal -> f (ElimX ext2 primTy primVal)
  }

type ExtTransformTE = ExtTransformTEF Identity

pattern Coerce :: Coercible a b => a -> b
pattern Coerce f <-
  (coerce -> f)
  where
    Coerce f = coerce f

pattern ExtTransformTE ::
  (XStar ext1 primTy primVal -> XStar ext2 primTy primVal) ->
  (XPrimTy ext1 primTy primVal -> XPrimTy ext2 primTy primVal) ->
  (XPrim ext1 primTy primVal -> XPrim ext2 primTy primVal) ->
  (XPi ext1 primTy primVal -> XPi ext2 primTy primVal) ->
  (XLam ext1 primTy primVal -> XLam ext2 primTy primVal) ->
  (XSig ext1 primTy primVal -> XSig ext2 primTy primVal) ->
  (XPair ext1 primTy primVal -> XPair ext2 primTy primVal) ->
  (XCatProduct ext1 primTy primVal -> XCatProduct ext2 primTy primVal) ->
  (XCatCoproduct ext1 primTy primVal -> XCatCoproduct ext2 primTy primVal) ->
  (XCatProductIntro ext1 primTy primVal -> XCatProductIntro ext2 primTy primVal) ->
  (XCatProductElimLeft ext1 primTy primVal -> XCatProductElimLeft ext2 primTy primVal) ->
  (XCatProductElimRight ext1 primTy primVal -> XCatProductElimRight ext2 primTy primVal) ->
  (XCatCoproductIntroLeft ext1 primTy primVal -> XCatCoproductIntroLeft ext2 primTy primVal) ->
  (XCatCoproductIntroRight ext1 primTy primVal -> XCatCoproductIntroRight ext2 primTy primVal) ->
  (XCatCoproductElim ext1 primTy primVal -> XCatCoproductElim ext2 primTy primVal) ->
  (XUnitTy ext1 primTy primVal -> XUnitTy ext2 primTy primVal) ->
  (XUnit ext1 primTy primVal -> XUnit ext2 primTy primVal) ->
  (XLet ext1 primTy primVal -> XLet ext2 primTy primVal) ->
  (XElim ext1 primTy primVal -> XElim ext2 primTy primVal) ->
  (XBound ext1 primTy primVal -> XBound ext2 primTy primVal) ->
  (XFree ext1 primTy primVal -> XFree ext2 primTy primVal) ->
  (XApp ext1 primTy primVal -> XApp ext2 primTy primVal) ->
  (XAnn ext1 primTy primVal -> XAnn ext2 primTy primVal) ->
  (TermX ext1 primTy primVal -> TermX ext2 primTy primVal) ->
  (ElimX ext1 primTy primVal -> ElimX ext2 primTy primVal) ->
  ExtTransformTE ext1 ext2 primTy primVal
pattern ExtTransformTE
  { etStar,
    etPrimTy,
    etPrim,
    etPi,
    etLam,
    etSig,
    etPair,
    etCatProduct,
    etCatCoproduct,
    etCatProductIntro,
    etCatProductElimLeft,
    etCatProductElimRight,
    etCatCoproductIntroLeft,
    etCatCoproductIntroRight,
    etCatCoproductElim,
    etUnitTy,
    etUnit,
    etLet,
    etElim,
    etBound,
    etFree,
    etApp,
    etAnn,
    etTermX,
    etElimX
  } =
  ExtTransformTEF
    { etfStar = Coerce etStar,
      etfPrimTy = Coerce etPrimTy,
      etfPrim = Coerce etPrim,
      etfPi = Coerce etPi,
      etfLam = Coerce etLam,
      etfSig = Coerce etSig,
      etfUnitTy = Coerce etUnitTy,
      etfUnit = Coerce etUnit,
      etfPair = Coerce etPair,
      etfCatProduct = Coerce etCatProduct,
      etfCatCoproduct = Coerce etCatCoproduct,
      etfCatProductIntro = Coerce etCatProductIntro,
      etfCatProductElimLeft = Coerce etCatProductElimLeft,
      etfCatProductElimRight = Coerce etCatProductElimRight,
      etfCatCoproductIntroLeft = Coerce etCatCoproductIntroLeft,
      etfCatCoproductIntroRight = Coerce etCatCoproductIntroRight,
      etfCatCoproductElim = Coerce etCatCoproductElim,
      etfLet = Coerce etLet,
      etfElim = Coerce etElim,
      etfBound = Coerce etBound,
      etfFree = Coerce etFree,
      etfApp = Coerce etApp,
      etfAnn = Coerce etAnn,
      etfTermX = Coerce etTermX,
      etfElimX = Coerce etElimX
    }

extTransformTF ::
  Applicative f =>
  ExtTransformTEF f ext1 ext2 primTy primVal ->
  Term ext1 primTy primVal ->
  f (Term ext2 primTy primVal)
extTransformTF fs (Star i e) = Star i <$> etfStar fs e
extTransformTF fs (PrimTy k e) = PrimTy k <$> etfPrimTy fs e
extTransformTF fs (Prim k e) = Prim k <$> etfPrim fs e
extTransformTF fs (Pi π s t e) =
  Pi π <$> extTransformTF fs s <*> extTransformTF fs t <*> etfPi fs e
extTransformTF fs (Lam t e) = Lam <$> extTransformTF fs t <*> etfLam fs e
extTransformTF fs (Sig π s t e) =
  Sig π <$> extTransformTF fs s <*> extTransformTF fs t <*> etfSig fs e
extTransformTF fs (Pair s t e) =
  Pair <$> extTransformTF fs s <*> extTransformTF fs t <*> etfPair fs e
extTransformTF fs (CatProduct s t e) =
  CatProduct <$> extTransformTF fs s <*> extTransformTF fs t <*> etfCatProduct fs e
extTransformTF fs (CatCoproduct s t e) =
  CatCoproduct <$> extTransformTF fs s <*> extTransformTF fs t <*> etfCatCoproduct fs e
extTransformTF fs (CatProductIntro s t e) =
  CatProductIntro <$> extTransformTF fs s <*> extTransformTF fs t <*> etfCatProductIntro fs e
extTransformTF fs (CatProductElimLeft a s e) =
  CatProductElimLeft <$> extTransformTF fs a <*> extTransformTF fs s <*> etfCatProductElimLeft fs e
extTransformTF fs (CatProductElimRight a s e) =
  CatProductElimRight <$> extTransformTF fs a <*> extTransformTF fs s <*> etfCatProductElimRight fs e
extTransformTF fs (CatCoproductIntroLeft s e) =
  CatCoproductIntroLeft <$> extTransformTF fs s <*> etfCatCoproductIntroLeft fs e
extTransformTF fs (CatCoproductIntroRight s e) =
  CatCoproductIntroRight <$> extTransformTF fs s <*> etfCatCoproductIntroRight fs e
extTransformTF fs (CatCoproductElim a b cp s t e) =
  CatCoproductElim <$> extTransformTF fs a <*> extTransformTF fs b <*> extTransformTF fs cp <*> extTransformTF fs s <*> extTransformTF fs t <*> etfCatCoproductElim fs e
extTransformTF fs (UnitTy e) =
  UnitTy <$> etfUnitTy fs e
extTransformTF fs (Unit e) =
  Unit <$> etfUnit fs e
extTransformTF fs (Let π l b e) =
  Let π <$> extTransformEF fs l <*> extTransformTF fs b <*> etfLet fs e
extTransformTF fs (Elim f e) = Elim <$> extTransformEF fs f <*> etfElim fs e
extTransformTF fs (TermX e) = TermX <$> etfTermX fs e

extTransformT ::
  ExtTransformTE ext1 ext2 primTy primVal ->
  Term ext1 primTy primVal ->
  Term ext2 primTy primVal
extTransformT fs t = runIdentity $ extTransformTF fs t

extTransformEF ::
  Applicative f =>
  ExtTransformTEF f ext1 ext2 primTy primVal ->
  Elim ext1 primTy primVal ->
  f (Elim ext2 primTy primVal)
extTransformEF fs (Bound x e) = Bound x <$> etfBound fs e
extTransformEF fs (Free x e) = Free x <$> etfFree fs e
extTransformEF fs (App f s e) =
  App <$> extTransformEF fs f
    <*> extTransformTF fs s
    <*> etfApp fs e
extTransformEF fs (Ann π s t e) =
  Ann π <$> extTransformTF fs s
    <*> extTransformTF fs t
    <*> etfAnn fs e
extTransformEF fs (ElimX e) = ElimX <$> etfElimX fs e

extTransformE ::
  ExtTransformTE ext1 ext2 primTy primVal ->
  Elim ext1 primTy primVal ->
  Elim ext2 primTy primVal
extTransformE fs t = runIdentity $ extTransformEF fs t

type ForgotExtTE ext primTy primVal =
  ( XStar ext primTy primVal ~ (),
    XPrimTy ext primTy primVal ~ (),
    XPrim ext primTy primVal ~ (),
    XPi ext primTy primVal ~ (),
    XSig ext primTy primVal ~ (),
    XPair ext primTy primVal ~ (),
    XCatProduct ext primTy primVal ~ (),
    XCatCoproduct ext primTy primVal ~ (),
    XCatProductIntro ext primTy primVal ~ (),
    XCatProductElimLeft ext primTy primVal ~ (),
    XCatProductElimRight ext primTy primVal ~ (),
    XCatCoproductIntroLeft ext primTy primVal ~ (),
    XCatCoproductIntroRight ext primTy primVal ~ (),
    XCatCoproductElim ext primTy primVal ~ (),
    XUnitTy ext primTy primVal ~ (),
    XUnit ext primTy primVal ~ (),
    XLam ext primTy primVal ~ (),
    XLet ext primTy primVal ~ (),
    XElim ext primTy primVal ~ (),
    XBound ext primTy primVal ~ (),
    XFree ext primTy primVal ~ (),
    XApp ext primTy primVal ~ (),
    XAnn ext primTy primVal ~ ()
  )

forgetterTE ::
  ( TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void,
    ForgotExtTE ext' primTy primVal
  ) =>
  ExtTransformTE ext ext' primTy primVal
forgetterTE =
  ExtTransformTE
    { etStar = const (),
      etPrimTy = const (),
      etPrim = const (),
      etPi = const (),
      etSig = const (),
      etPair = const (),
      etCatProduct = const (),
      etCatCoproduct = const (),
      etCatProductIntro = const (),
      etCatProductElimLeft = const (),
      etCatProductElimRight = const (),
      etCatCoproductIntroLeft = const (),
      etCatCoproductIntroRight = const (),
      etCatCoproductElim = const (),
      etUnitTy = const (),
      etUnit = const (),
      etLam = const (),
      etLet = const (),
      etElim = const (),
      etBound = const (),
      etFree = const (),
      etApp = const (),
      etAnn = const (),
      etTermX = absurd,
      etElimX = absurd
    }

extForgetT ::
  ( TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void,
    ForgotExtTE ext' primTy primVal
  ) =>
  Term ext primTy primVal ->
  Term ext' primTy primVal
extForgetT = extTransformT forgetterTE

extForgetE ::
  ( TermX ext primTy primVal ~ Void,
    ElimX ext primTy primVal ~ Void,
    ForgotExtTE ext' primTy primVal
  ) =>
  Elim ext primTy primVal ->
  Elim ext' primTy primVal
extForgetE = extTransformE forgetterTE

composeTE ::
  Monad f =>
  ExtTransformTEF f ext2 ext3 primTy primVal ->
  ExtTransformTEF f ext1 ext2 primTy primVal ->
  ExtTransformTEF f ext1 ext3 primTy primVal
composeTE fs gs =
  ExtTransformTEF
    { etfStar = etfStar fs <=< etfStar gs,
      etfPrimTy = etfPrimTy fs <=< etfPrimTy gs,
      etfPrim = etfPrim fs <=< etfPrim gs,
      etfPi = etfPi fs <=< etfPi gs,
      etfSig = etfSig fs <=< etfSig gs,
      etfPair = etfPair fs <=< etfPair gs,
      etfCatProduct = etfCatProduct fs <=< etfCatProduct gs,
      etfCatCoproduct = etfCatCoproduct fs <=< etfCatCoproduct gs,
      etfCatProductIntro = etfCatProductIntro fs <=< etfCatProductIntro gs,
      etfCatProductElimLeft = etfCatProductElimLeft fs <=< etfCatProductElimLeft gs,
      etfCatProductElimRight = etfCatProductElimRight fs <=< etfCatProductElimRight gs,
      etfCatCoproductIntroLeft = etfCatCoproductIntroLeft fs <=< etfCatCoproductIntroLeft gs,
      etfCatCoproductIntroRight = etfCatCoproductIntroRight fs <=< etfCatCoproductIntroRight gs,
      etfCatCoproductElim = etfCatCoproductElim fs <=< etfCatCoproductElim gs,
      etfUnitTy = etfUnitTy fs <=< etfUnitTy gs,
      etfUnit = etfUnit fs <=< etfUnit gs,
      etfLam = etfLam fs <=< etfLam gs,
      etfLet = etfLet fs <=< etfLet gs,
      etfElim = etfElim fs <=< etfElim gs,
      etfBound = etfBound fs <=< etfBound gs,
      etfFree = etfFree fs <=< etfFree gs,
      etfApp = etfApp fs <=< etfApp gs,
      etfAnn = etfAnn fs <=< etfAnn gs,
      etfTermX = etfTermX fs <=< etfTermX gs,
      etfElimX = etfElimX fs <=< etfElimX gs
    }


data ExtTransformVNF f ext1 ext2 primTy primVal = ExtTransformVNF
  { etfVStar :: XVStar ext1 primTy primVal -> f (XVStar ext2 primTy primVal),
    etfVPrimTy :: XVPrimTy ext1 primTy primVal -> f (XVPrimTy ext2 primTy primVal),
    etfVPi :: XVPi ext1 primTy primVal -> f (XVPi ext2 primTy primVal),
    etfVLam :: XVLam ext1 primTy primVal -> f (XVLam ext2 primTy primVal),
    etfVSig :: XVSig ext1 primTy primVal -> f (XVSig ext2 primTy primVal),
    etfVPair :: XVPair ext1 primTy primVal -> f (XVPair ext2 primTy primVal),
    etfVCatProduct :: XVCatProduct ext1 primTy primVal -> f (XVCatProduct ext2 primTy primVal),
    etfVCatCoproduct :: XVCatCoproduct ext1 primTy primVal -> f (XVCatCoproduct ext2 primTy primVal),
    etfVCatProductIntro :: XVCatProductIntro ext1 primTy primVal -> f (XVCatProductIntro ext2 primTy primVal),
    etfVCatProductElimLeft :: XVCatProductElimLeft ext1 primTy primVal -> f (XVCatProductElimLeft ext2 primTy primVal),
    etfVCatProductElimRight :: XVCatProductElimRight ext1 primTy primVal -> f (XVCatProductElimRight ext2 primTy primVal),
    etfVCatCoproductIntroLeft :: XVCatCoproductIntroLeft ext1 primTy primVal -> f (XVCatCoproductIntroLeft ext2 primTy primVal),
    etfVCatCoproductIntroRight :: XVCatCoproductIntroRight ext1 primTy primVal -> f (XVCatCoproductIntroRight ext2 primTy primVal),
    etfVCatCoproductElim :: XVCatCoproductElim ext1 primTy primVal -> f (XVCatCoproductElim ext2 primTy primVal),
    etfVUnitTy :: XVUnitTy ext1 primTy primVal -> f (XVUnitTy ext2 primTy primVal),
    etfVUnit :: XVUnit ext1 primTy primVal -> f (XVUnit ext2 primTy primVal),
    etfVNeutral :: XVNeutral ext1 primTy primVal -> f (XVNeutral ext2 primTy primVal),
    etfVPrim :: XVPrim ext1 primTy primVal -> f (XVPrim ext2 primTy primVal),
    etfNBound :: XNBound ext1 primTy primVal -> f (XNBound ext2 primTy primVal),
    etfNFree :: XNFree ext1 primTy primVal -> f (XNFree ext2 primTy primVal),
    etfNApp :: XNApp ext1 primTy primVal -> f (XNApp ext2 primTy primVal),
    etfValueX :: ValueX ext1 primTy primVal -> f (ValueX ext2 primTy primVal),
    etfNeutralX :: NeutralX ext1 primTy primVal -> f (NeutralX ext2 primTy primVal)
  }

type ExtTransformVN = ExtTransformVNF Identity

pattern ExtTransformVN ::
  (XVStar ext1 primTy primVal -> XVStar ext2 primTy primVal) ->
  (XVPrimTy ext1 primTy primVal -> XVPrimTy ext2 primTy primVal) ->
  (XVPi ext1 primTy primVal -> XVPi ext2 primTy primVal) ->
  (XVLam ext1 primTy primVal -> XVLam ext2 primTy primVal) ->
  (XVSig ext1 primTy primVal -> XVSig ext2 primTy primVal) ->
  (XVPair ext1 primTy primVal -> XVPair ext2 primTy primVal) ->
  (XVCatProduct ext1 primTy primVal -> XVCatProduct ext2 primTy primVal) ->
  (XVCatCoproduct ext1 primTy primVal -> XVCatCoproduct ext2 primTy primVal) ->
  (XVCatProductIntro ext1 primTy primVal -> XVCatProductIntro ext2 primTy primVal) ->
  (XVCatProductElimLeft ext1 primTy primVal -> XVCatProductElimLeft ext2 primTy primVal) ->
  (XVCatProductElimRight ext1 primTy primVal -> XVCatProductElimRight ext2 primTy primVal) ->
  (XVCatCoproductIntroLeft ext1 primTy primVal -> XVCatCoproductIntroLeft ext2 primTy primVal) ->
  (XVCatCoproductIntroRight ext1 primTy primVal -> XVCatCoproductIntroRight ext2 primTy primVal) ->
  (XVCatCoproductElim ext1 primTy primVal -> XVCatCoproductElim ext2 primTy primVal) ->
  (XVUnitTy ext1 primTy primVal -> XVUnitTy ext2 primTy primVal) ->
  (XVUnit ext1 primTy primVal -> XVUnit ext2 primTy primVal) ->
  (XVNeutral ext1 primTy primVal -> XVNeutral ext2 primTy primVal) ->
  (XVPrim ext1 primTy primVal -> XVPrim ext2 primTy primVal) ->
  (XNBound ext1 primTy primVal -> XNBound ext2 primTy primVal) ->
  (XNFree ext1 primTy primVal -> XNFree ext2 primTy primVal) ->
  (XNApp ext1 primTy primVal -> XNApp ext2 primTy primVal) ->
  (ValueX ext1 primTy primVal -> ValueX ext2 primTy primVal) ->
  (NeutralX ext1 primTy primVal -> NeutralX ext2 primTy primVal) ->
  ExtTransformVN ext1 ext2 primTy primVal
pattern ExtTransformVN
  { etVStar,
    etVPrimTy,
    etVPi,
    etVLam,
    etVSig,
    etVPair,
    etVCatProduct,
    etVCatCoproduct,
    etVCatProductIntro,
    etVCatProductElimLeft,
    etVCatProductElimRight,
    etVCatCoproductIntroLeft,
    etVCatCoproductIntroRight,
    etVCatCoproductElim,
    etVUnitTy,
    etVUnit,
    etVNeutral,
    etVPrim,
    etNBound,
    etNFree,
    etNApp,
    etValueX,
    etNeutralX
  } =
  ExtTransformVNF {
      etfVStar = Coerce etVStar,
      etfVPrimTy = Coerce etVPrimTy,
      etfVPi = Coerce etVPi,
      etfVLam = Coerce etVLam,
      etfVSig = Coerce etVSig,
      etfVPair = Coerce etVPair,
      etfVCatProduct = Coerce etVCatProduct,
      etfVCatCoproduct = Coerce etVCatCoproduct,
      etfVCatProductIntro = Coerce etVCatProductIntro,
      etfVCatProductElimLeft = Coerce etVCatProductElimLeft,
      etfVCatProductElimRight = Coerce etVCatProductElimRight,
      etfVCatCoproductIntroLeft = Coerce etVCatCoproductIntroLeft,
      etfVCatCoproductIntroRight = Coerce etVCatCoproductIntroRight,
      etfVCatCoproductElim = Coerce etVCatCoproductElim,
      etfVUnitTy = Coerce etVUnitTy,
      etfVUnit = Coerce etVUnit,
      etfVNeutral = Coerce etVNeutral,
      etfVPrim = Coerce etVPrim,
      etfNBound = Coerce etNBound,
      etfNFree = Coerce etNFree,
      etfNApp = Coerce etNApp,
      etfValueX = Coerce etValueX,
      etfNeutralX = Coerce etNeutralX
    }

extTransformVF ::
  Applicative f =>
  ExtTransformVNF f ext1 ext2 primTy primVal ->
  Value ext1 primTy primVal ->
  f (Value ext2 primTy primVal)
extTransformVF fs (VStar ℓ e) = VStar ℓ <$> etfVStar fs e
extTransformVF fs (VPrimTy p e) = VPrimTy p <$> etfVPrimTy fs e
extTransformVF fs (VPrim p e) = VPrim p <$> etfVPrim fs e
extTransformVF fs (VPi π a b e) =
  VPi π <$> extTransformVF fs a <*> extTransformVF fs b <*> etfVPi fs e
extTransformVF fs (VLam t e) =
  VLam <$> extTransformVF fs t <*> etfVLam fs e
extTransformVF fs (VSig π a b e) =
  VSig π <$> extTransformVF fs a <*> extTransformVF fs b <*> etfVSig fs e
extTransformVF fs (VPair s t e) =
  VPair <$> extTransformVF fs s <*> extTransformVF fs t <*> etfVPair fs e
extTransformVF fs (VCatProduct a b e) =
  VCatProduct
    <$> extTransformVF fs a
    <*> extTransformVF fs b
    <*> etfVCatProduct fs e
extTransformVF fs (VCatCoproduct a b e) =
  VCatCoproduct
    <$> extTransformVF fs a
    <*> extTransformVF fs b
    <*> etfVCatCoproduct fs e
extTransformVF fs (VCatProductIntro s t e) =
  VCatProductIntro
    <$> extTransformVF fs s
    <*> extTransformVF fs t
    <*> etfVCatProductIntro fs e
extTransformVF fs (VCatProductElimLeft b s e) =
  VCatProductElimLeft
    <$> extTransformVF fs b
    <*> extTransformVF fs s
    <*> etfVCatProductElimLeft fs e
extTransformVF fs (VCatProductElimRight a t e) =
  VCatProductElimRight
    <$> extTransformVF fs a
    <*> extTransformVF fs t
    <*> etfVCatProductElimRight fs e
extTransformVF fs (VCatCoproductIntroLeft s e) =
  VCatCoproductIntroLeft
    <$> extTransformVF fs s
    <*> etfVCatCoproductIntroLeft fs e
extTransformVF fs (VCatCoproductIntroRight t e) =
  VCatCoproductIntroRight
    <$> extTransformVF fs t
    <*> etfVCatCoproductIntroRight fs e
extTransformVF fs (VCatCoproductElim a b s t u e) =
  VCatCoproductElim
    <$> extTransformVF fs a
    <*> extTransformVF fs b
    <*> extTransformVF fs s
    <*> extTransformVF fs t
    <*> extTransformVF fs u
    <*> etfVCatCoproductElim fs e
extTransformVF fs (VUnitTy e) = VUnitTy <$> etfVUnitTy fs e
extTransformVF fs (VUnit e) = VUnit <$> etfVUnit fs e
extTransformVF fs (VNeutral n e) =
  VNeutral <$> extTransformNF fs n <*> etfVNeutral fs e
extTransformVF fs (ValueX e) =
  ValueX <$> etfValueX fs e

extTransformV ::
  ExtTransformVN ext1 ext2 primTy primVal ->
  Value ext1 primTy primVal ->
  Value ext2 primTy primVal
extTransformV fs = runIdentity . extTransformVF fs

extTransformNF ::
  Applicative f =>
  ExtTransformVNF f ext1 ext2 primTy primVal ->
  Neutral ext1 primTy primVal ->
  f (Neutral ext2 primTy primVal)
extTransformNF fs (NBound i e) =
  NBound i <$> etfNBound fs e
extTransformNF fs (NFree x e) =
  NFree x <$> etfNFree fs e
extTransformNF fs (NApp f s e) =
  NApp
    <$> extTransformNF fs f
    <*> extTransformVF fs s
    <*> etfNApp fs e
extTransformNF fs (NeutralX e) =
  NeutralX <$> etfNeutralX fs e

extTransformN ::
  ExtTransformVN ext1 ext2 primTy primVal ->
  Neutral ext1 primTy primVal ->
  Neutral ext2 primTy primVal
extTransformN fs = runIdentity . extTransformNF fs

