{-# LANGUAGE UndecidableInstances #-}

module Juvix.Backends.Interpreter.Parameterization
  ( interpreter,
  )
where

import qualified GHC.Natural as Natural
import qualified Juvix.Backends.Interpreter.Primitive as Primitive
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.IR.Evaluator as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
  ( Applicative (pure),
    Bool (..),
    Double,
    Eq ((==)),
    Foldable (length),
    Integer,
    Maybe (..),
    Monoid (mempty),
    NonEmpty (..),
    Text,
    const,
    fromInteger,
    ($),
    (.),
  )
import qualified Juvix.Library.HashMap as HashMap
import qualified Prelude

instance Param.CanPrimApply Param.Star Primitive.PrimTy where
  primArity Primitive.PrimBool = 0
  primArity Primitive.PrimNat = 0
  primArity Primitive.PrimInt = 0
  primArity Primitive.PrimFloat = 0
  primArity Primitive.PrimString = 0
  primArity Primitive.PrimList = 1
  primArity Primitive.PrimSexp = 1
  primArity Primitive.PrimFunc = 2

  primApply = Prelude.error "Interpreter: PrimTy.primApply not yet implemented"

instance Param.CanPrimApply Primitive.PrimTy Primitive.RawPrimVal where
  primArity = Prelude.error "Interpreter: RawPrimVal.primArity not yet implemented"

  primApply = Prelude.error "Interpreter: RawPrimVal.primApply not yet implemented"

interpreter :: Param.Parameterisation Primitive.PrimTy Primitive.RawPrimVal
interpreter =
  Param.Parameterisation
    { Param.hasType = hasType,
      Param.builtinTypes = builtinTypes,
      Param.builtinValues = builtinValues,
      Param.stringVal = stringToRawPrimVal,
      Param.intVal = integerToRawPrimVal,
      Param.floatVal = floatToRawPrimVal
    }
  where
    hasType :: Primitive.RawPrimVal -> Param.PrimType Primitive.PrimTy -> Bool
    hasType val (Param.PrimType ty) = hasType' val ty

    hasType' :: Primitive.RawPrimVal -> NonEmpty Primitive.PrimTy -> Bool
    hasType' (Primitive.BoolVal _) (Primitive.PrimBool :| []) = True
    hasType' (Primitive.NatVal _) (Primitive.PrimNat :| []) = True
    hasType' (Primitive.IntVal _) (Primitive.PrimInt :| []) = True
    hasType' (Primitive.FloatVal _) (Primitive.PrimFloat :| []) = True
    hasType' (Primitive.StringVal _) (Primitive.PrimString :| []) = True
    hasType'
      Primitive.PrimIntToNat
      (Primitive.PrimInt :| [Primitive.PrimNat]) = True
    hasType'
      Primitive.PrimAdd
      (Primitive.PrimInt :| [Primitive.PrimInt, Primitive.PrimInt]) = True
    hasType'
      Primitive.PrimMul
      (Primitive.PrimInt :| [Primitive.PrimInt, Primitive.PrimInt]) = True
    hasType'
      Primitive.PrimAppend
      (Primitive.PrimString :| [Primitive.PrimString, Primitive.PrimString]) = True
    hasType'
      Primitive.PrimNth
      (Primitive.PrimNat :| [Primitive.PrimString, Primitive.PrimString]) = True
    hasType'
      Primitive.PrimLength
      (Primitive.PrimString :| [Primitive.PrimNat]) = True
    hasType'
      Primitive.PrimIntEq
      (Primitive.PrimInt :| [Primitive.PrimInt, Primitive.PrimBool]) = True
    hasType'
      Primitive.PrimIntLt
      (Primitive.PrimInt :| [Primitive.PrimInt, Primitive.PrimBool]) = True
    hasType'
      Primitive.PrimStringEq
      (Primitive.PrimString :| [Primitive.PrimString, Primitive.PrimBool]) = True
    hasType'
      Primitive.PrimIntIf
      ( Primitive.PrimBool
          :| [Primitive.PrimInt, Primitive.PrimInt, Primitive.PrimInt]
        ) = True
    hasType'
      Primitive.PrimStringIf
      ( Primitive.PrimBool
          :| [Primitive.PrimString, Primitive.PrimString, Primitive.PrimString]
        ) = True
    hasType'
      Primitive.PrimAnd
      (Primitive.PrimBool :| [Primitive.PrimBool, Primitive.PrimBool]) = True
    hasType'
      Primitive.PrimOr
      (Primitive.PrimBool :| [Primitive.PrimBool, Primitive.PrimBool]) = True
    hasType'
      Primitive.PrimNot
      (Primitive.PrimBool :| [Primitive.PrimBool]) = True
    hasType' _ _ = False

    builtinTypes :: Param.Builtins Primitive.PrimTy
    builtinTypes =
      HashMap.fromList
        [ ("Interpreter.bool", Primitive.PrimBool),
          ("Interpreter.nat", Primitive.PrimNat),
          ("Interpreter.int", Primitive.PrimInt),
          ("Interpreter.float", Primitive.PrimFloat),
          ("Interpreter.string", Primitive.PrimString),
          ("Interpreter.list", Primitive.PrimList),
          ("Interpreter.sexp", Primitive.PrimSexp),
          ("Interpreter.func", Primitive.PrimFunc)
        ]

    builtinValues :: Param.Builtins Primitive.RawPrimVal
    builtinValues =
      HashMap.fromList
        [ ("Interpreter.true", Primitive.BoolVal True),
          ("Interpreter.false", Primitive.BoolVal False),
          ("Interpreter.nil", Primitive.ListNil),
          ("Interpreter.cons", Primitive.ListCons),
          ("Interpreter.snil", Primitive.SexpNil),
          ("Interpreter.scons", Primitive.SexpCons),
          ("Interpreter.satom", Primitive.SexpAtom),
          ("Interpreter.id", Primitive.PrimIdentity),
          ("Interpreter.compose", Primitive.PrimCompose),
          ("Interpreter.int-to-nat", Primitive.PrimIntToNat),
          ("Interpreter.add", Primitive.PrimAdd),
          ("Interpreter.mul", Primitive.PrimMul),
          ("Interpreter.append", Primitive.PrimAppend),
          ("Interpreter.nth", Primitive.PrimNth),
          ("Interpreter.length", Primitive.PrimLength),
          ("Interpreter.int-eq", Primitive.PrimIntEq),
          ("Interpreter.int-lt", Primitive.PrimIntLt),
          ("Interpreter.string-eq", Primitive.PrimStringEq),
          ("Interpreter.int-if", Primitive.PrimIntIf),
          ("Interpreter.string-if", Primitive.PrimStringIf),
          ("Interpreter.and", Primitive.PrimAnd),
          ("Interpreter.or", Primitive.PrimOr),
          ("Interpreter.not", Primitive.PrimNot)
        ]

    integerToRawPrimVal :: Integer -> Maybe Primitive.RawPrimVal
    integerToRawPrimVal = Just . Primitive.IntVal

    stringToRawPrimVal :: Text -> Maybe Primitive.RawPrimVal
    stringToRawPrimVal = Just . Primitive.StringVal

    floatToRawPrimVal :: Double -> Maybe Primitive.RawPrimVal
    floatToRawPrimVal = Just . Primitive.FloatVal

instance IR.HasWeak Primitive.PrimTy where weakBy' _ _ t = t

instance IR.HasWeak Primitive.RawPrimVal where weakBy' _ _ t = t

instance
  Monoid (Core.XVPrimTy ext Primitive.PrimTy primVal) =>
  IR.HasSubstValueType ext Primitive.PrimTy primVal Primitive.PrimTy
  where
  substValueTypeWith _ _ _ t = pure $ Core.VPrimTy t mempty

instance
  Monoid (Core.XPrimTy ext Primitive.PrimTy primVal) =>
  IR.HasPatSubstType ext Primitive.PrimTy primVal Primitive.PrimTy
  where
  patSubstType' _ _ t = pure $ Core.PrimTy t mempty

instance
  Monoid (Core.XPrim ext primTy Primitive.RawPrimVal) =>
  IR.HasPatSubstTerm ext primTy Primitive.RawPrimVal Primitive.RawPrimVal
  where
  patSubstTerm' _ _ t = pure $ Core.Prim t mempty
