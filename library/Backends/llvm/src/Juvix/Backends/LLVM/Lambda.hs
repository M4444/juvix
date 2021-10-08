{-# LANGUAGE NamedFieldPuns #-}

{- File that contains a number of functions related to the lambda-calculus and
 - lifting. -}

module Juvix.Backends.LLVM.Lambda where

import Juvix.Backends.LLVM.Globals
import Juvix.Backends.LLVM.Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage
import qualified Prelude as P

-- lambdaLift ::
--   Map.Map NameSymbol.T (ErasedAnn.Type PrimTy) ->
--   ErasedAnn.AnnTerm PrimTy RawPrimVal ->
--   ErasedAnn.AnnTerm PrimTy RawPrimVal
-- lambdaLift tyEnv t@(ErasedAnn.Ann usage ty t') = case t' of
--   ErasedAnn.LamM {ErasedAnn.capture, ErasedAnn.arguments, ErasedAnn.body} -> do
--     let params = capture <> arguments
--         ty' = mkFunctionType ({-Map.elems tyEnv-} <> argTys <> [retTy]) -- ErasedAnn.Pi usage undefined ty -- TODO: prepend captured types
--         body' = lambdaLift tyEnv' body
--         t'' =
--           ErasedAnn.Ann usage ty' $
--             ErasedAnn.LamM
--               { ErasedAnn.capture = mempty,
--                 ErasedAnn.arguments = params,
--                 ErasedAnn.body = body'
--               }
--         (argTys, retTy) = functionType ty
--         argsCaptured = Map.toList $ Map.filterWithKey (\k _ -> k `elem` capture) tyEnv
--         argsParams = zip arguments argTys
--         tyEnv' = Map.fromList argsParams `Map.union` tyEnv
--         args = map (\(k, v) -> ErasedAnn.Ann usage v $ ErasedAnn.Var k) (argsCaptured <> argsParams)
--      in ErasedAnn.Ann usage ty $
--           ErasedAnn.AppM t'' args
--   ErasedAnn.AppM f xs ->
--     let f' = lambdaLift tyEnv f
--         xs' = map (lambdaLift tyEnv) xs
--      in ErasedAnn.Ann usage ty $ ErasedAnn.AppM f' xs'
--   ErasedAnn.Var _ -> t
--   ErasedAnn.Prim _ -> t

-- | Implementation based on rewriting the captures using an app + lambda over
-- the original lambda. This does not work correctly, as afterwards the
-- captures are still captured.
lambdaLift ::
  Map.Map NameSymbol.T (ErasedAnn.Type PrimTy) ->
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  ErasedAnn.AnnTerm PrimTy RawPrimVal
lambdaLift tyEnv t@(ErasedAnn.Ann usage ty t') = case t' of
  ErasedAnn.LamM {ErasedAnn.capture, ErasedAnn.arguments, ErasedAnn.body} ->
    case capture of
      [] ->
        let body' = lambdaLift tyEnv' body
            tyEnv' = Map.fromList (zip arguments argTys) `Map.union` tyEnv
            (argTys, _) = functionType ty
         in ErasedAnn.Ann usage ty $ t' {ErasedAnn.body = body'}
      _ : _ ->
        let params = capture
            ty' = mkFunctionType (captureTys <> [retTy])
            body' = lambdaLift tyEnv' body
            t'' =
              ErasedAnn.Ann usage ty $
                ErasedAnn.LamM
                  { ErasedAnn.capture = mempty, -- Remove the captures
                    ErasedAnn.arguments = arguments,
                    ErasedAnn.body = body'
                  }
            captureLam =
              ErasedAnn.Ann one ty' $
                ErasedAnn.LamM
                  { ErasedAnn.capture = mempty,
                    ErasedAnn.arguments = params,
                    ErasedAnn.body = t''
                  }
            captureTys :: [ErasedAnn.Type PrimTy]
            captureTys = catMaybes $ map (flip Map.lookup tyEnv) capture
            captureArgs = zip capture captureTys
            args = map (\(name, ty) -> ErasedAnn.Ann usage ty $ ErasedAnn.Var name) captureArgs
            tyEnv' = Map.fromList (zip arguments argTys) `Map.union` tyEnv
            (argTys, retTy) = functionType ty
         in ErasedAnn.Ann usage ty $ ErasedAnn.AppM captureLam args
  ErasedAnn.AppM f xs ->
    let f' = lambdaLift tyEnv f
        xs' = map (lambdaLift tyEnv) xs
     in ErasedAnn.Ann usage ty $ ErasedAnn.AppM f' xs'
  ErasedAnn.Var _ -> t
  ErasedAnn.Prim _ -> t

mkFunctionType :: [ErasedAnn.Type PrimTy] -> ErasedAnn.Type PrimTy
mkFunctionType [] = P.error "Can not make an empty type."
mkFunctionType (ty : []) = ty
mkFunctionType (ty : tys) = ErasedAnn.Pi Usage.SAny ty (mkFunctionType tys)

lambdaExtract ::
  ( HasState "globals" (GlobalMap PrimTy RawPrimVal) m,
    HasState "nameGen" Natural m
  ) =>
  ErasedAnn.AnnTerm PrimTy RawPrimVal ->
  m (ErasedAnn.AnnTerm PrimTy RawPrimVal)
lambdaExtract t@(ErasedAnn.Ann usage ty t') = case t' of
  ErasedAnn.LamM {ErasedAnn.body, ErasedAnn.arguments} -> do
    name <- freshGlobalName "lam"
    traceShowM ("lambda-extract", name)
    body' <- lambdaExtract body
    let t'' = ErasedAnn.Ann usage ty $ t' {ErasedAnn.body = body'}
    modify @"globals" $ Map.insert name t''
    return $ ErasedAnn.Ann usage ty $ ErasedAnn.Var name
  ErasedAnn.AppM f xs -> do
    f' <- lambdaExtract f
    xs' <- mapM lambdaExtract xs
    return $ ErasedAnn.Ann usage ty $ ErasedAnn.AppM f' xs'
  ErasedAnn.Var _ -> return t
  ErasedAnn.Prim _ -> return t

-- | Construct a tuple of the types of the argument and return type of a function
-- type.
functionType ::
  ErasedAnn.Type primTy ->
  ([ErasedAnn.Type primTy], ErasedAnn.Type primTy)
functionType ty = (init tys, P.last tys)
  where
    tys = functionType' ty
    functionType' (ErasedAnn.Pi usage l r) = l : functionType' r
    functionType' ty = [ty]
