{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The basic connection between the backend and the Juvix pipeline.
module Juvix.Backends.Interpreter.Pipeline
  ( BInterpreter (..),
  )
where

import qualified Data.Aeson as A
import qualified Juvix.Backends.Interpreter.Compilation as Compiliation
import qualified Juvix.Backends.Interpreter.Parameterization as Parameterization
import qualified Juvix.Backends.Interpreter.Primitive as Primitive
import qualified Juvix.Core.Erased.Ann as ErasedAnn
import Juvix.Library
import qualified Juvix.Pipeline as Pipeline

-- | Identifier for the Interpreter backend.
data BInterpreter = BInterpreter
  deriving (Show, Eq, Generic, A.ToJSON, A.FromJSON)

instance Pipeline.HasBackend BInterpreter where
  type Ty BInterpreter = Primitive.PrimTy
  type Val BInterpreter = Primitive.RawPrimVal
  type Err BInterpreter = Primitive.CompilationError

  stdlibs _ = ["Interpreter.ju"]

  param _t = Parameterization.interpreter

  typecheck ctx = Pipeline.typecheck' ctx Parameterization.interpreter

  compile' term = do
    let raw = ErasedAnn.toRaw term
    Compiliation.compileProgram raw
