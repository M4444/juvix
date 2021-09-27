{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}

module Juvix.Core.IR.Typechecker.API.Types where

import Juvix.Library
import Juvix.Core.IR.Typechecker.Env as Env
import qualified Juvix.Core.Base.Types as Core
import qualified Juvix.Core.Types as Core
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.IR.Typechecker.API.History as Hist
import qualified Juvix.Core.IR.Typechecker as Typed
-- import qualifief Juvix.Core.Unification as Uni

-- | This API is based on Idris's v1 proof state
-- | Check "Idris, a General Purpose Dependently Typed Programming Language"
-- | for more information

data Error primTy primVal
  = UnificationErr   -- Uni.UnificationErr
  | HolesErr         -- Uni.HolesErr
  | LocalContextErr  -- Uni.TermContextErr
  | GlobalContextErr -- TODO: create specific context error, currently it's "fromFrontendError"
  | MetaProgErr      MetaProgErr
  | TypecheckerErr   (Typed.TypecheckError primTy primVal)
  | PrimError        -- Generic Primitive Error?

 -- `a` is a placeholder for the actual types
data ProofState ext primTy primVal a b = Env
  { -- captures all changes to proof term
    history        :: Hist.History a b,

    -- unsolved unification problems
    -- requires unification
    -- TODO: check with Andy how the unification goal works

    proofGoal      :: a, -- Uni.UnificationGoal primTy primVal,

    -- current proof
    -- TODO: check with Andy how to deal with bidirectional typecheking here
    proofTerm      :: Core.Term ext primTy primVal,

     -- global context with all types
    globals        :: Core.RawGlobal ext primTy primVal,

    -- local context of `proofTerm`
    -- requires unification
    locals         :: a, -- Uni.TermConxtext primTy primVal,

    -- queue of current holes and guesses
    -- requires unification
    holes          :: () -- Uni.Holes primTy primVal
  }
  deriving (Generic)

type ProofStateA primTy primVal a b =
  ExceptT
    (Error primTy primVal a)
    (State (ProofState IR.T primTy primVal a b))

newtype ProofStateT primTy primVal sumRep a b c
  = ProofStateT (ProofStateA primTy primVal sumRep a b c)
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState  "history" (Hist.History a b),
      HasSink   "history" (Hist.History a b),
      HasSource "history" (Hist.History a b)
    )
    via StateField "history" (ProofStateA primTy primVal sumRep a b)
  -- deriving
  --   ( HasState  "globals" (),
  --     HasSink   "globals" (),
  --     HasSource "globals" ()
  --   )
  --   via StateField "history" (ProofStateA primTy primVal sumRep a b)
  -- deriving
  --   ( HasState "proofTerm" (),
  --     HasSink "proofTerm" (),
  --     HasSource "proofTerm" ()
  --   )
  --   via StateField "proofTerm" (ProofStateA primTy primVal sumRep a b)
  -- deriving
  --   ( HasState "proofGoal" (),
  --     HasSink "proofGoal" (),
  --     HasSource "proofGoal" ()
  --   )
  --   via StateField "" (ProofStateA primTy primVal sumRep a b)
  -- deriving
  --   ( HasState "locals" (),
  --     HasSink "locals" (),
  --     HasSource "locals" ()
  --   )
  --   via StateField "" (ProofStateA primTy primVal sumRep a b)
  -- deriving
  --   ( HasState "holes" (),
  --     HasSink "holes" (),
  --     HasSource "holes" ()
  --   )
  -- via StateField "" (ProofStateA primTy primVal sumRep a b)
  deriving
    (HasThrow "proofErr" (Error primTy primVal))
    via MonadError (ProofStateA primTy primVal sumRep a b)

data MetaProgError
  = MetaProgError -- not sure what errors can happen here, but will leave a placeholder
