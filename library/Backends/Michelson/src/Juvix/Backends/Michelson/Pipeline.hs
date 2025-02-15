{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Juvix.Backends.Michelson.Pipeline (BMichelson (..), compileMichelson) where

import qualified Data.Aeson as A
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.Pipeline as Pipeline

data BMichelson = BMichelson
  deriving (Show, Eq, Generic, A.ToJSON, A.FromJSON)

instance HasBackend BMichelson where
  type Ty BMichelson = Param.RawPrimTy
  type Val BMichelson = Param.RawPrimVal
  type Err BMichelson = Param.CompilationError

  stdlibs _ =
    [ "Michelson/Prelude.ju",
      "Michelson/Alias.ju"
    ]

  param _ = Param.michelson

  typecheck ctx = Pipeline.typecheck' ctx Param.michelson

  compile' term = do
    let (res, _logs) = M.compileContract term
    case res of
      Right c -> do
        pure $ M.untypedContractToSource (fst c)
      Left err -> Feedback.fail $ show err

compileMichelson ::
  MonadFail f =>
  ErasedAnn.AnnTermT Param.RawPrimTy Param.RawPrimVal ->
  f Text
compileMichelson term = do
  let (res, _logs) = M.compileContract term
  case res of
    Right c -> pure $ M.untypedContractToSource (fst c)
    Left err -> Feedback.fail $ show err
