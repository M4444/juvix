module Juvix.Core.CaseTree where

import qualified Data.Map as Map
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Core.Base as Core
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.IR.Evaluator as IR

-- Inputs of the algorithm

-- 1. A signature Sigma containing previous declarations, as well as clauses fot the branches of the case tree that have already been checked
-- 2. A context T containing the types of the pattern variables: dom(T) = PV(q)
-- The function f currently being checked
-- The copatterns q for the current branch of the case tree
-- The refined target type C of the current branch
-- The user input P, which is described below

-- The outputs of the algorithm are a signature Sigma' extending Sigma with new clauses and a well-typed case tree Q such that `Simga; T |- f q := Q : C ~> Sigma'`


-- We represent the user input P to the algorithm as an ordered list of partially decomposed clauses, called a left-hand side problem. Each partially decomposed claused is of the form:
-- [E]q -> rhs, where E is an (unordered) set of constraints



-- TODO: Tackle case with one argument
clausesToCaseTrees 
    :: ( IR.EvalPatSubst ext primTy primVal,
         IR.NoExtensions ext primTy primVal,
         IR.ShowAllV ext primTy primVal,
         Monad m
    ) 
    => Core.RawFunction ext primTy primVal
    -> IR.LookupFun ext primTy primVal
    -> m (Core.Term ext primTy primVal)
clausesToCaseTrees (Core.RawFunction name usage ty clauses) lookupFun = do 
    case ty of
        Core.Pi u t1 t2 ann ->
            let (v, ty') = introduceVar t1
                cs = caseSplitOnVar v clauses
            in notImplemented 
            -- Core.Pi u (inlineAllGlobals t1 lookupFun patternMap) (inlineAllGlobals t2 lookupFun patternMap) ann
    where
        go = notImplemented 
        introduceVar = notImplemented
        caseSplitOnVar = notImplemented 
-- Inputs of the algorithm

-- 1. A signature Sigma containing previous declarations, as well as clauses fot the branches of the case tree that have already been checked
-- 2. A context T containing the types of the pattern variables: dom(T) = PV(q)
-- The function f currently being checked
-- The copatterns q for the current branch of the case tree
-- The refined target type C of the current branch
-- The user input P, which is described below

-- The outputs of the algorithm are a signature Sigma' extending Sigma with new clauses and a well-typed case tree Q such that `Simga; T |- f q := Q : C ~> Sigma'`


-- We represent the user input P to the algorithm as an ordered list of partially decomposed clauses, called a left-hand side problem. Each partially decomposed claused is of the form:
-- [E]q -> rhs, where E is an (unordered) set of constraints
