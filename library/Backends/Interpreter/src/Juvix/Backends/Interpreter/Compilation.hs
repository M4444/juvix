{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

module Juvix.Backends.Interpreter.Compilation
  ( compileProgram,
    extractFunctionType,
  )
where

import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Data.Aeson as Aeson
import qualified Juvix.Backends.Interpreter.Primitive as Primitive
import qualified Juvix.Core.Categorial as Categorial
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
  ( Bool (..),
    Bounded,
    Data,
    Double,
    Either (..),
    Enum,
    Eq,
    Generic,
    Hashable,
    Int,
    Integer,
    Maybe (..),
    Monad,
    NFData,
    Natural,
    Ord,
    Read,
    Show,
    Text,
    Typeable,
    elem,
    filter,
    length,
    mapM,
    null,
    return,
    show,
    unless,
    unlines,
    zip,
    ($),
    (*),
    (+),
    (++),
    (.),
    (<>),
    (==),
    (>>=),
  )
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp.Serialize as Serialize
import qualified Safe.Exact as Safe
import Prelude
  ( String,
    lookup,
  )

--------------------------------------------------------------------------------
-- Interpreter utilities
--------------------------------------------------------------------------------

newtype Environment termType = Env [Binding termType]
  deriving stock
    ( Read,
      Show,
      Eq,
      Ord,
      Generic,
      Typeable,
      Data
    )
  deriving newtype
    ( NFData,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey
    )
  deriving anyclass
    ( Serialize.DefaultOptions,
      Serialize.Serialize
    )

type Closure termType = (termType, Environment termType)

type Binding termType = (NameSymbol.T, Closure termType)

--------------------------------------------------------------------------------
-- Entry point from Core for compilation/evaluation
--------------------------------------------------------------------------------

type ErasedAnnTerm = ErasedAnn.AnnTerm Primitive.PrimTy Primitive.RawPrimVal

type ErasedType = ErasedAnn.Type Primitive.PrimTy

compileProgram ::
  Monad m =>
  ErasedAnnTerm ->
  Feedback.FeedbackT [] String m Text
compileProgram term = do
  i <- ExceptT.runExceptT $ interpret [] term
  case i of
    Right interpretedVal -> do
      reduced <- ExceptT.runExceptT $ reduce ([], []) interpretedVal
      case reduced of
        Right reducedVal ->
          return $
            unlines
              [ "Generated code:",
                show interpretedVal,
                "Result of execution:",
                show reducedVal
              ]
        Left err ->
          Feedback.fail $ show err
    Left err -> Feedback.fail $ show err

--------------------------------------------------------------------------------
-- Compilation of Core terms to a simple Haskell lambda calculus
--------------------------------------------------------------------------------

data InterpretedVal
  = InterpretedBool Bool
  | InterpretedNat Natural
  | InterpretedInt Integer
  | InterpretedFloat Double
  | InterpretedString Text
  | InterpretedPair InterpretedVal InterpretedVal
  | InterpretedApply InterpretedVal [InterpretedVal]
  | InterpretedLambda [NameSymbol.T] InterpretedVal
  | InterpretedVar NameSymbol.T
  | InterpretedTypedVar NameSymbol.T ErasedType
  | InterpretedAtom InterpretedVal
  | InterpretedIntToNat
  | InterpretedAdd
  | InterpretedMul
  | InterpretedAppend
  | InterpretedNth
  | InterpretedLength
  | InterpretedNatEq
  | InterpretedStringEq
  | InterpretedNatIf
  | InterpretedNatLt
  | InterpretedStringIf
  | InterpretedAnd
  | InterpretedOr
  | InterpretedNot
  deriving
    ( Read,
      Show,
      Eq,
      Generic,
      Typeable,
      Data,
      NFData,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey,
      Serialize.DefaultOptions,
      Serialize.Serialize
    )

type TypeEnvironment = [(NameSymbol.T, ErasedType)]

data InterpretationError
  = NotImplementedYet Text TypeEnvironment ErasedAnnTerm
  | CodegenNotImplementedYet Text TypeEnvironment InterpretedVal
  | WrongFunctionTypeArgs TypeEnvironment ErasedAnnTerm
  | CategorialError TypeEnvironment (Categorial.CodegenError ErasedAnnTerm)
  | NonFunctionType TypeEnvironment ErasedType
  | InterpretedFreeVariable TypeEnvironment ErasedAnnTerm
  deriving
    ( Read,
      Show,
      Eq,
      Generic,
      Typeable,
      Data,
      NFData,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey
    )
  deriving anyclass
    ( Serialize.DefaultOptions,
      Serialize.Serialize
    )

type InterpretResultT m = ExceptT.ExceptT InterpretationError m InterpretedVal

codegenFunctions ::
  Monad m =>
  TypeEnvironment ->
  Categorial.CodegenFunctions
    (ExceptT.ExceptT InterpretationError m)
    InterpretedVal
    ErasedAnnTerm
codegenFunctions env =
  Categorial.CodegenFunctions
    { Categorial.genObj = obj,
      Categorial.genAtom = atom,
      Categorial.genFunc = gf,
      Categorial.genIdentity = gi,
      Categorial.genCompose = gc
    }
  where
    obj :: Monad m => ErasedAnnTerm -> InterpretResultT m
    obj = interpret env

    atom :: Monad m => InterpretedVal -> InterpretResultT m
    atom = return . InterpretedAtom

    gf ::
      Monad m =>
      Maybe (InterpretedVal, InterpretedVal) ->
      ErasedAnnTerm ->
      InterpretResultT m
    gf _maybeSignature = interpret env

    gi :: Monad m => Maybe InterpretedVal -> InterpretResultT m
    gi _maybeType = return $ InterpretedLambda [var] $ InterpretedVar var
      where
        var :: NameSymbol.T
        var = "v"

    gc :: Monad m => InterpretedVal -> InterpretedVal -> InterpretResultT m
    gc g f =
      return $
        InterpretedLambda [var] $
          InterpretedApply g [InterpretedApply f [InterpretedVar var]]
      where
        var :: NameSymbol.T
        var = "v"

extractFunctionType ::
  Monad m =>
  TypeEnvironment ->
  ErasedType ->
  ExceptT.ExceptT InterpretationError m ([ErasedType], ErasedType)
extractFunctionType env (ErasedAnn.Pi _usage domain codomain) = case codomain of
  ErasedAnn.Pi _usage' domain' codomain' -> do
    (argTys, retTy) <- extractFunctionType env codomain
    return (domain' : argTys, retTy)
  nonFunctionTy -> return ([domain], nonFunctionTy)
extractFunctionType env ty =
  ExceptT.throwE $ NonFunctionType env ty

interpret ::
  Monad m =>
  TypeEnvironment ->
  ErasedAnnTerm ->
  InterpretResultT m
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim (Primitive.IntVal i))) =
  return $ InterpretedInt i
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim (Primitive.StringVal s))) =
  return $ InterpretedString s
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim (Primitive.BoolVal b))) =
  return $ InterpretedBool b
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimAdd)) =
  return InterpretedAdd
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimMul)) =
  return InterpretedMul
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimAppend)) =
  return InterpretedAppend
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimLength)) =
  return InterpretedLength
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimNth)) =
  return InterpretedNth
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimIntEq)) =
  return InterpretedNatEq
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimIntLt)) =
  return InterpretedNatLt
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimStringEq)) =
  return InterpretedStringEq
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimIntIf)) =
  return InterpretedNatIf
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimStringIf)) =
  return InterpretedStringIf
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimAnd)) =
  return InterpretedAnd
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimOr)) =
  return InterpretedOr
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimNot)) =
  return InterpretedNot
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.Prim Primitive.PrimIntToNat)) =
  return InterpretedIntToNat
interpret env (ErasedAnn.Ann _ _ (ErasedAnn.CatProductIntroM fst snd)) = do
  ifst <- interpret env fst
  isnd <- interpret env snd
  return $ InterpretedPair ifst isnd
interpret env _term@(ErasedAnn.Ann _ _ (ErasedAnn.CategorialTermM catTerm)) = do
  codeOrErr <-
    ExceptT.runExceptT $ Categorial.generateCode (codegenFunctions env) catTerm
  case codeOrErr of
    Left err -> ExceptT.throwE $ CategorialError env err
    Right code -> return code
interpret env term@(ErasedAnn.Ann _ _ (ErasedAnn.AppM f x)) = do
  f' <- interpret env f
  x' <- mapM (interpret env) x
  return $ InterpretedApply f' x'
interpret env term@(ErasedAnn.Ann _ ty (ErasedAnn.LamM _caps args body)) = do
  -- Core currently does not have a multi-argument pi type, so instead
  -- of using extractFunctionType here, we just assume there's only one
  -- argument in the lambda term.
  argType <- case ty of
    ErasedAnn.Pi _usage domain codomain ->
      return domain
    _ ->
      ExceptT.throwE $ NonFunctionType env ty
  let argTypes = [argType]
  unless (length args == length argTypes) $
    ExceptT.throwE $ WrongFunctionTypeArgs env term
  body' <- interpret (zip args argTypes ++ env) body
  return $ InterpretedLambda args body'
interpret env term@(ErasedAnn.Ann _ ty (ErasedAnn.Var var)) = do
  case lookup var env of
    Just ty -> return $ InterpretedTypedVar var ty
    Nothing -> ExceptT.throwE $ InterpretedFreeVariable env term
interpret env term =
  ExceptT.throwE $ NotImplementedYet "Interpreter::interpret" env term

--------------------------------------------------------------------------------
-- Reduction (evaluation) of Haskell lambda calculus terms
--------------------------------------------------------------------------------

-- Free and bound variables.
type ReductionEnvironment = ([NameSymbol.T], [(NameSymbol.T, InterpretedVal)])

data ReductionError
  = OnlyOneArgumentLambdasSupported ReductionEnvironment InterpretedVal
  | UnboundVariable NameSymbol.T ReductionEnvironment
  | ReducedUntypedVariable NameSymbol.T ReductionEnvironment
  deriving
    ( Read,
      Show,
      Eq,
      Generic,
      Typeable,
      Data,
      NFData,
      Aeson.ToJSON,
      Aeson.FromJSON,
      Aeson.ToJSONKey,
      Aeson.FromJSONKey
    )
  deriving anyclass
    ( Serialize.DefaultOptions,
      Serialize.Serialize
    )

type ReductionResultT m = ExceptT.ExceptT ReductionError m InterpretedVal

reduce ::
  Monad m =>
  ReductionEnvironment ->
  InterpretedVal ->
  ReductionResultT m
reduce env val@(InterpretedApply f args) = do
  f' <- reduce env f
  args' <- mapM (reduce env) args
  case (f', args') of
    (InterpretedAdd, [InterpretedInt i1, InterpretedInt i2]) ->
      return $ InterpretedInt $ i1 + i2
    (InterpretedMul, [InterpretedInt i1, InterpretedInt i2]) ->
      return $ InterpretedInt $ i1 * i2
    (InterpretedLambda vars body, _) -> do
      case (vars, args') of
        ([var], [arg']) -> do
          let (free, bound) = env
          reduce (free, (var, arg') : bound) body
        _ -> ExceptT.throwE $ OnlyOneArgumentLambdasSupported env f
    _ ->
      return $ InterpretedApply f' args'
reduce env (InterpretedVar var) =
  ExceptT.throwE $ ReducedUntypedVariable var env
reduce env@(free, bound) term@(InterpretedTypedVar var _ty) =
  case lookup var bound of
    Just val -> return val
    Nothing ->
      if var `elem` free
        then return term
        else ExceptT.throwE $ UnboundVariable var env
reduce (free, bound) (InterpretedLambda vars body) = do
  reduced <- reduce (vars ++ free, bound) body
  return $ InterpretedLambda vars reduced
reduce env (InterpretedPair fst snd) = do
  fst' <- reduce env fst
  snd' <- reduce env snd
  return $ InterpretedPair fst' snd'
reduce env (InterpretedAtom a) = do
  a' <- reduce env a
  return $ InterpretedAtom a'
reduce _env val = return val
