{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.IR.Typechecker.Error
  ( TypecheckError' (..),
    TypecheckError,
    HasThrowTC',
    HasThrowTC,
    throwTC,
  )
where

import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Typechecker.Types
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Translate
import Juvix.Library
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.Usage as Usage

data TypecheckError' extV extT primTy primVal
  = TypeMismatch
      { typeSubject :: Core.Elim extT primTy primVal,
        typeExpected, typeGot :: ValueT extV primTy primVal
      }
  | UniverseMismatch
      { universeLower, universeHigher :: Core.Universe
      }
  | ShouldBeStar
      { typeActual :: ValueT extV primTy primVal
      }
  | ShouldBeFunctionType
      { typeActual :: ValueT extV primTy primVal
      }
  | ShouldBePairType
      { typeActual :: ValueT extV primTy primVal
      }
  | ShouldBeCatProductType
      { typeActual :: ValueT extV primTy primVal
      }
  | ShouldBeCatCoproductType
      { typeActual :: ValueT extV primTy primVal
      }
  | ShouldBeUnitType
      { typeActual :: ValueT extV primTy primVal
      }
  | LeftoverUsage
      { usageLeftover :: Usage.T
      }
  | InsufficientUsage
      { usageNeeded, usageActual :: Usage.T
      }
  | UnboundLocal
      { unboundIndex :: Core.BoundVar
      }
  | UnboundGlobal
      { unboundGlobal :: Core.GlobalName
      }
  | UnboundPatVar
      { unboundPatVar :: Core.PatternVar
      }
  | NotPrimTy
      { typeActual :: ValueT extV primTy primVal
      }
  | WrongPrimTy
      { primVal :: primVal,
        primTy :: P.PrimType primTy
      }
  | UnsupportedTermExt
      { termExt :: Core.TermX extT primTy primVal
      }
  | UnsupportedElimExt
      { elimExt :: Core.ElimX extT primTy primVal
      }
  | PartiallyAppliedConstructor
      { pattern_ :: Core.Pattern extT primTy primVal
      }
  | EvalError
      { evalErr :: Eval.Error IR.T T (P.KindedType primTy) (P.TypedPrim primTy primVal)
      }
  | -- | datatype typechecking errors
    DatatypeError
      { invalidType :: Core.Term extT primTy primVal
      }
  | ConTypeError
      { invalidConTy :: ValueT extV primTy primVal
      }
  | ParamError
      { expectedN :: Core.GlobalName,
        exp :: Core.Term extT primTy primVal
      }
  | DeclError
      { tg :: Core.Term extT primTy primVal,
        name :: Core.GlobalName,
        tel :: Core.RawTelescope extT primTy primVal
      }

type TypecheckError = TypecheckError' IR.T IR.T

deriving instance
  ( Eq primTy,
    Eq primVal,
    Eq (P.PrimApplyError primTy),
    Eq (P.PrimApplyError primVal),
    Core.ValueAll Eq extV (P.KindedType primTy) (P.TypedPrim primTy primVal),
    Core.NeutralAll Eq extV (P.KindedType primTy) (P.TypedPrim primTy primVal),
    Core.TermAll Eq extT primTy primVal,
    Core.ElimAll Eq extT primTy primVal,
    Core.PatternAll Eq extT primTy primVal
  ) =>
  Eq (TypecheckError' extV extT primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    Show (P.PrimApplyError primTy),
    Show (P.PrimApplyError primVal),
    Core.ValueAll Show extV (P.KindedType primTy) (P.TypedPrim primTy primVal),
    Core.NeutralAll Show extV (P.KindedType primTy) (P.TypedPrim primTy primVal),
    Core.TermAll Show extT primTy primVal,
    Core.ElimAll Show extT primTy primVal,
    Core.PatternAll Show extT primTy primVal
  ) =>
  Show (TypecheckError' extV extT primTy primVal)

type instance PP.Ann (TypecheckError' IR.T IR.T _ _) = HR.PPAnn

type Doc = HR.Doc

deriving anyclass instance
  ( Show primTy,
    Show primVal,
    PP.PrettyText primTy,
    PP.PrettyText primVal
  ) =>
  PP.PrettyText (Prim primTy primVal)

deriving anyclass instance PP.PrettyText P.Star

-- TODO generalise
instance
  ( Show primTy,
    Show primVal,
    HR.PrimPretty primTy primVal,
    Eval.ApplyErrorPretty primTy (P.TypedPrim primTy primVal),
    PP.PrettyText (P.PrimApplyError primTy),
    HR.ToPPAnn (PP.Ann (P.PrimApplyError primTy))
  ) =>
  PP.PrettyText (TypecheckError' IR.T IR.T primTy primVal)
  where
  prettyT = \case
    TypeMismatch term exp got ->
      PP.sepIndent'
        [ (False, "Type mismatch at term"),
          (True, prettyHR $ IR.Elim term),
          (False, "expected type:"),
          (True, prettyVal exp),
          (False, "actual type"),
          (True, prettyVal got)
        ]
    UniverseMismatch lo hi ->
      PP.sep
        [ PP.hsep ["Universe", prettySA lo],
          PP.hsep ["should be less than", prettySA hi]
        ]
    ShouldBeStar ty -> expected "a type" ty
    ShouldBeFunctionType ty -> expected "a function" ty
    ShouldBePairType ty -> expected "a pair" ty
    ShouldBeCatProductType ty -> expected "a categorical product" ty
    ShouldBeCatCoproductType ty -> expected "a categorical coproduct" ty
    ShouldBeUnitType ty -> expected "a unit" ty
    LeftoverUsage π ->
      -- TODO: leftover usage of what???
      PP.hsep ["Usage", prettySA π, "left over"]
    InsufficientUsage πn πa ->
      -- TODO: insufficient usage of what???
      PP.sep
        [ PP.hsep ["Usage", prettySA πn, "needed but"],
          PP.hsep ["only", prettySA πa, "left over"]
        ]
    UnboundLocal i ->
      PP.hsep ["Unbound local variable", PP.annotate' HR.AName $ PP.show i]
    UnboundGlobal x ->
      PP.hsep
        [ "Name",
          PP.annotate' HR.AName $ PP.noAnn $ PP.prettyT x,
          "not in scope"
        ]
    UnboundPatVar i ->
      PP.hsep ["Unbound pattern variable", PP.annotate' HR.AName $ PP.show i]
    NotPrimTy ty ->
      PP.sepIndent'
        [ (False, "Not a primitive type:"),
          (True, prettyVal ty)
        ]
    WrongPrimTy val ty ->
      PP.sepIndent'
        [ (False, "Primitive value"),
          (True, HR.toPPAnn <$> PP.pretty0 val),
          (False, "cannot be given the type"),
          (True, HR.toPPAnn <$> PP.pretty0 ty)
        ]
    UnsupportedTermExt x -> absurd x -- TODO when generalised
    UnsupportedElimExt x -> absurd x -- TODO when generalised
    PartiallyAppliedConstructor pat ->
      PP.sepIndent'
        [ (False, "Pattern"),
          (True, PP.pretty0 $ fst $ irPatternToHR pat),
          (False, "contains an partially-applied constructor")
        ]
    EvalError err ->
      PP.prettyT err
    DatatypeError ty ->
      PP.sepIndent'
        [ (False, "Invalid type for datatype"),
          (True, prettyHR ty),
          (False, "The type of a datatype must be zero or more function"),
          (False, "types, ending in * i.")
        ]
    ConTypeError ty ->
      PP.sepIndent'
        [ (False, "Invalid type for data constructor:"),
          (True, prettyVal ty),
          (False, "The type of a datatype must be zero or more function"),
          (False, "types, ending in the datatype.")
        ]
    ParamError n tm ->
      PP.sepIndent'
        [ (False, "Invalid value of parameter"),
          (True, prettyHR tm),
          (False, "instead of the name " <> (HR.toPPAnn <$> PP.pretty0 n))
        ]
    DeclError tg _name _tel ->
      PP.sepIndent'
        [ (False, "Invalid target for data constructor:"),
          (True, prettyHR tg),
          (False, "The type of a datatype must be zero or more function"),
          (False, "types, ending in the datatype.")
        ]
    where
      expected what ty =
        PP.sepIndent'
          [ (False, PP.hsep ["Expected", what, "but got a term of type"]),
            (True, prettyVal ty)
          ]

prettySA :: Show a => a -> Doc
prettySA = PP.annotate' HR.ATyCon . PP.show

prettyVal ::
  ( Show primTy,
    Show primVal,
    PP.PrettyText primTy,
    PP.PrettyText primVal,
    HR.PrimPretty primTy primVal
  ) =>
  IR.Value primTy primVal ->
  Doc
prettyVal = prettyHR . Core.quote

prettyHR ::
  ( Show primTy,
    Show primVal,
    PP.PrettyText primTy,
    PP.PrettyText primVal,
    HR.PrimPretty primTy primVal
  ) =>
  IR.Term primTy primVal ->
  Doc
prettyHR = PP.pretty0 . irToHR

type HasThrowTC' extV extT primTy primVal m =
  HasThrow "typecheckError" (TypecheckError' extV extT primTy primVal) m

type HasThrowTC primTy primVal m =
  HasThrowTC' IR.T IR.T primTy primVal m

throwTC ::
  HasThrowTC' extV extT primTy primVal m =>
  TypecheckError' extV extT primTy primVal ->
  m z
throwTC = throw @"typecheckError"
