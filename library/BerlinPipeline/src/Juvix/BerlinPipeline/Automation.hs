{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Juvix.BerlinPipeline.Automation where

import qualified Juvix.BerlinPipeline.Pipeline as Pipeline
import qualified Juvix.Context as Context
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------

data T
  = ProcessJob ProcessJobNoEnv
  | UpdateJob
      { newContext :: Context.T Sexp.T Sexp.T Sexp.T,
        process :: ProcessJob
      }
  deriving (Show)

type Job = T

class HasExtract a m | a -> m where
  extract :: a x -> m (Pipeline.COut x)

----------------------------------------
-- Output Types
----------------------------------------

data Stage
  = Current
  | FromTopToCurrent
  | Eval
  deriving (Show, Eq)

data ProcessJob = Process
  { current :: Pipeline.EnvOrSexp,
    newForms :: [(Stage, Pipeline.EnvOrSexp)]
  }
  deriving (Show)

data ProcessJobNoEnv = ProcessNoEnv
  { current :: Sexp.T,
    newForms :: [(Stage, Sexp.T)]
  }
  deriving (Show)

promoteSimpleForms :: [(Stage, Sexp.T)] -> [(Stage, Pipeline.EnvOrSexp)]
promoteSimpleForms = fmap (second Pipeline.Sexp)

promoteNoEnvToEnv :: ProcessJobNoEnv -> ProcessJob
promoteNoEnvToEnv (ProcessNoEnv {current, newForms}) =
  Process
    { newForms = promoteSimpleForms newForms,
      current = Pipeline.Sexp current
    }

----------------------------------------
-- Input Types
----------------------------------------

data PassArgument = PassArgument
  { current :: Pipeline.EnvOrSexp,
    context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

data SimplifiedPassArgument = SimplifiedArgument
  { current :: Sexp.T,
    context :: Context.T Sexp.T Sexp.T Sexp.T
  }
  deriving (Show)

simplify ::
  HasThrow "error" Text m => (SimplifiedPassArgument -> m Job) -> PassArgument -> m Job
simplify f (PassArgument {current, context}) =
  case current of
    Pipeline.Sexp sexp ->
      f (simplified sexp)
    Pipeline.InContext name -> do
      let update updateBody sexpTerm =
            updateTerm name updateBody context sexpTerm
      case context Context.!? name of
        Just def -> do
          -- Abstract this out, improve the interface to the
          -- Context... why is it this complicated?
          let ourDef = Context.extractValue def
          case ourDef of
            Context.Def d@(Context.D {defTerm}) -> do
              sexpTerm <- f (simplified defTerm)
              update (\sexp -> d {Context.defTerm = sexp} |> Context.Def) sexpTerm
            Context.TypeDeclar typ -> do
              sexpTerm <- f (simplified typ)
              update Context.TypeDeclar sexpTerm
            Context.SumCon s@Context.Sum {sumTDef} ->
              -- Does sumTName need to be updated?
              case sumTDef of
                Just d@(Context.D {defTerm}) -> do
                  sexpTerm <- f (simplified defTerm)
                  update
                    ( \sexp ->
                        d {Context.defTerm = sexp}
                          |> (\newDef -> s {Context.sumTDef = Just newDef})
                          |> Context.SumCon
                    )
                    sexpTerm
                Nothing ->
                  -- What does it mean for the Sum constructor not to have a sumTDef?
                  noOpJob current context |> pure
        Nothing ->
          throw @"error"
            "Could not find definition when \
            \ it was promised to be in the environment"
  where
    simplified sexp = (SimplifiedArgument {current = sexp, context})

    noOpJob current context =
      UpdateJob
        { newContext = context,
          process = Process {current = current, newForms = []}
        }

updateTerm ::
  HasThrow "error" Text f =>
  NameSymbol.T ->
  (Sexp.T -> Context.Definition Sexp.T Sexp.T Sexp.T) ->
  Context.T Sexp.T Sexp.T Sexp.T ->
  T ->
  f T
updateTerm name rePackageTerm context job = do
  (sexp, context, newForms) <-
    case job of
      ProcessJob (ProcessNoEnv {current, newForms}) ->
        (current, context, promoteSimpleForms newForms) |> pure
      UpdateJob {newContext, process = (Process {current, newForms})} ->
        case current of
          Pipeline.Sexp sexp ->
            (sexp, newContext, newForms) |> pure
          Pipeline.InContext name ->
            throw @"error" $
              "Attempting to redefine term already in env" <> NameSymbol.toText name
  UpdateJob
    { newContext = Context.addGlobal name (rePackageTerm sexp) context,
      process =
        Process
          { current = Pipeline.InContext name,
            newForms
          }
    }
    |> pure

-- | @extractProcessJob@ Will extract the promoted ProcessJob that
-- talks about the environment, even if the job is a @ProcessJob@ by
-- simply wrapping it.
extractProcessJob :: Job -> ProcessJob
extractProcessJob (UpdateJob {process}) = process
extractProcessJob (ProcessJob process) = promoteNoEnvToEnv process
