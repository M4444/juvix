{-# LANGUAGE NamedFieldPuns #-}

module Juvix.Backends.LLVM.Compilation
  ( compileProgram,
  )
where

import qualified Data.String as S (fromString)
import Juvix.Backends.LLVM.Globals
import Juvix.Backends.LLVM.Lambda
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import Juvix.Library.Feedback
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified LLVM.AST as LLVM (Module, Name, Operand (..), mkName)
import qualified LLVM.AST.Constant as LLVM (Constant (..))
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder as LLVM
import qualified LLVM.Pretty as LLVM
import Text.Pretty.Simple (pPrint)
import qualified Prelude as P

mkGlobals ::
  LLVM.MonadModuleBuilder m =>
  Env ->
  GlobalMap PrimTy RawPrimVal ->
  m ()
mkGlobals env globals =
  traverse_ mkGlobal (Map.toList globals)
  where
    mkGlobal (name, t@(ErasedAnn.Ann usage ty t')) = case t' of
      ErasedAnn.LamM {ErasedAnn.arguments, ErasedAnn.body} ->
        void $ mkLam env name ty body arguments []
      _ -> P.error "Shouldn't happen"

-- | A mapping between Juvix variable names and their operands in LLVM.
type Env = Map.Map NameSymbol.T LLVM.Operand

-- | Compile the input program to an LLVM module.
-- TODO: maybe do something smarter with the module name?
compileProgram ::
  ( Monad m,
    MonadIO m -- TODO: remove!
  ) =>
  -- | Term to compile.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  FeedbackT [] P.String m Text
compileProgram t = do
  let t' = lambdaLift mempty t
  let (t'', Globals globals n) = runGlobalState (lambdaExtract t') (Globals mempty 0)
  print "=== input"
  pPrint t
  print "=== lifted"
  pPrint t'
  print "=== extracted"
  pPrint t''
  print "=== globals"
  pPrint globals
  let env :: Env
      env = Map.mapWithKey (\k (ErasedAnn.Ann usage ty _) -> globalRef (typeToLLVM ty) (LLVM.mkName $ S.fromString $ unintern $ NameSymbol.toSymbol k)) globals
  liftIO $ pPrint env
  let llvmmod = LLVM.buildModule "juvix-module" $ do
        mkGlobals env globals
  -- mkMain env t''
  return $ toStrict $ LLVM.ppllvm llvmmod

-- | Write the main function of the module. Here two distinct cases can be
-- observed:
--
-- * The term @t@ is a lambda abstraction; this is the case when the main
-- function takes one or more arguments. Within the scope of the input
-- program, this lambda is never applied, as this only happens when the program
-- is executed.
-- A global function is written for the lambda and called in the main function.
-- The arguments for the call are passed on from main.
--
-- * The term @t@ is any other term than a lambda abstraction, we can write the
-- body of the main function by compiling @t@. The main function itself does
-- not have any parameters in this case.
mkMain ::
  LLVM.MonadModuleBuilder m =>
  Env ->
  -- | Term to compile.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  m LLVM.Operand
mkMain env t@(ErasedAnn.Ann usage ty t') = do
  let (paramTys, returnTy) = functionType ty
      returnTy' = typeToLLVM returnTy
      paramTys' = map typeToLLVM paramTys
      paramNames = repeat "arg" :: [NameSymbol.T]
      params = zip paramTys' (map mkParameterName paramNames)
  LLVM.function "main" params returnTy' $ \args -> do
    out <- case t' of
      -- ErasedAnn.LamM
      --   { ErasedAnn.capture,
      --     ErasedAnn.arguments,
      --     ErasedAnn.body
      --   } -> do
      --     let env' = (Map.fromList $ zip paramNames args) <> env
      --         callArgs = zip args (repeat []) -- No arg attributes.
      --         -- funname <- mkLam env ty body arguments capture
      --     let funname = "main-lam"
      --     P.error "main-lam"
      --     LLVM.call (globalRef (typeToLLVM ty) funname) callArgs
      _ -> compileTerm env t
    LLVM.ret out

-- | Compile a term to its equivalent LLVM code.
-- TODO: implement other constructors of ErasedAnn.Term
compileTerm ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) =>
  -- | Environment of Juvix variables to LLVM function arguments.
  Env ->
  -- | The term to compile.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  m LLVM.Operand
compileTerm env (ErasedAnn.Ann usage ty t) = case t of
  ErasedAnn.Var symbol -> case Map.lookup symbol env of
    -- Nothing -> LLVM.call (globalRef (typeToLLVM ty) (LLVM.mkName $ S.fromString $ unintern $ NameSymbol.toSymbol symbol)) []
    Nothing -> P.error $ "Variable not found: " <> show symbol <> " in " <> show env -- TODO improve error message.
    Just var -> return var
  ErasedAnn.Prim t' -> mkPrim t ty
  ErasedAnn.AppM f xs -> mkApp env f ty xs

-- | Write an LLVM function definition based on a the given lambda abstraction.
-- The function returns the name of the create function.
mkLam ::
  ( LLVM.MonadModuleBuilder m
  ) =>
  -- | Environment of Juvix variables to LLVM function arguments.
  Env ->
  NameSymbol.T ->
  -- | The type of the lambda abstraction.
  ErasedAnn.Type PrimTy ->
  -- | The body of the lambda abstraction.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  -- | List of parameter names.
  [NameSymbol.T] ->
  -- | List of captures variables (free variables for the body).
  [NameSymbol.T] ->
  m LLVM.Name
mkLam env funname ty body args capt = do
  let funname' = LLVM.mkName $ S.fromString $ unintern $ NameSymbol.toSymbol funname
      (argTys, returnTy) = functionType ty
      returnTy' = typeToLLVM returnTy
      paramNames = repeat "arg"
      params = zipWith mkParameter argTys paramNames
  LLVM.function funname' params returnTy' $ \refs -> do
    let env' = env `Map.union` Map.fromList (zip args refs)
    body' <- compileTerm env' body
    LLVM.ret body'
  return funname'

-- | Given a juvix name and type, construct an llvm function parameter
-- definition.
mkParameter ::
  -- | The type of the parameter.
  ErasedAnn.Type PrimTy ->
  -- | The name of the parameter.
  NameSymbol.T ->
  (LLVM.Type, LLVM.ParameterName)
mkParameter ty name = (typeToLLVM ty, mkParameterName name)

-- | The function assumes the arguments passed are the arguments of an
-- application.
mkApp ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) =>
  -- | Environment of Juvix variables to LLVM function arguments.
  Env ->
  -- | The function term of an application.
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  -- | The type of the application.
  ErasedAnn.Type PrimTy ->
  -- | The arguments to the application.
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  m LLVM.Operand
mkApp env f@(ErasedAnn.Ann {ErasedAnn.term, ErasedAnn.type'}) _ xs =
  case term of
    -- ErasedAnn.LamM {ErasedAnn.body, ErasedAnn.arguments, ErasedAnn.capture} -> do
    --   funname <- mkLam env type' body arguments capture
    --   -- let funname = "lamapp" -- undefined
    --   xs' <- mapM (compileTerm env) xs
    --   let xs'args = zip xs' (repeat [])
    --   LLVM.call (globalRef (typeToLLVM type') funname) xs'args
    ErasedAnn.Prim prim -> applyPrim env prim xs
    ErasedAnn.Var v -> do
      f' <- compileTerm env f
      xs' <- mapM (compileTerm env) xs
      let xs'args = zip xs' (repeat []) -- Do not pass attributes to the args.
      LLVM.call f' xs'args
    ErasedAnn.AppM _ _ -> do
      f' <- compileTerm env f
      xs' <- mapM (compileTerm env) xs
      let xs'args = zip xs' (repeat []) -- Do not pass attributes to the args.
      LLVM.call f' xs'args

-- | Write LLVM code for a primitive.
-- TODO: implement other primitives.
mkPrim ::
  LLVM.MonadIRBuilder m =>
  -- | Term that contains the primitive.
  ErasedAnn.Term PrimTy RawPrimVal ->
  -- | Type of the primitive.
  ErasedAnn.Type PrimTy ->
  m LLVM.Operand
mkPrim (ErasedAnn.Prim prim) ty = case prim of
  LitInt i -> case ty of
    ErasedAnn.PrimTy (PrimTy LLVM.IntegerType {LLVM.typeBits}) ->
      return $
        LLVM.ConstantOperand $
          LLVM.Int {LLVM.integerBits = typeBits, LLVM.integerValue = i}

-- | Generate code for primitives that are used as a function.
-- TODO: implement other primitives.
applyPrim ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) =>
  -- | Environment of global variables.
  Env ->
  -- | The function primitive of the application.
  RawPrimVal ->
  -- | The arguments to the application.
  [ErasedAnn.AnnTerm PrimTy RawPrimVal] ->
  m LLVM.Operand
applyPrim env f xs
  | arityRaw f == lengthN xs =
    case f of
      Add -> do
        x <- compileTerm env (xs P.!! 0)
        y <- compileTerm env (xs P.!! 1)
        LLVM.add x y

-- | Counterpart of `mkName`: make a `ParameterName` given a name.
mkParameterName :: NameSymbol.T -> LLVM.ParameterName
mkParameterName s = S.fromString $ unintern $ NameSymbol.toSymbol s

-- | Handy wrapper for creating global references based on a type and name.
globalRef :: LLVM.Type -> LLVM.Name -> LLVM.Operand
globalRef ty name = LLVM.ConstantOperand $ LLVM.GlobalReference ty name

-- | Translate a Juvix type into an LLVM type.
typeToLLVM :: ErasedAnn.Type PrimTy -> LLVM.Type
typeToLLVM (ErasedAnn.PrimTy (PrimTy ty)) = ty
typeToLLVM ty@(ErasedAnn.Pi _usage _f _xs) =
  LLVM.FunctionType
    { LLVM.resultType = typeToLLVM resultType,
      LLVM.argumentTypes = map typeToLLVM argumentTypes,
      LLVM.isVarArg = False
    }
  where
    (argumentTypes, resultType) = functionType ty
