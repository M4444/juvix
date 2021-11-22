module Juvix.Core.CaseTrees where

import qualified Data.Map as Map
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Core.Base as Core
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.IR.Evaluator as IR
-- TODO: Tackle case with one argument
clausesToCaseTrees 
    :: ( IR.EvalPatSubst ext primTy primVal,
         IR.NoExtensions ext primTy primVal,
         IR.ShowAllV ext primTy primVal,
         Monad m
    ) 
    => Core.RawFunction ext primTy primVal
    -> IR.LookupFun ext primTy primVal
    -> m (Core.RawFunction ext primTy primVal)
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

data WithArity c = WithArity { arity :: Int, content :: c }
  deriving (Data, Functor, Foldable, Traversable, Show, Generic)

-- -- | Branches in a case tree.

data Case c = Branches
  { projPatterns   :: Bool
    -- ^ We are constructing a record here (copatterns).
    --   'conBranches' lists projections.
  , conBranches    :: Map NameSymbol.T (WithArity c)
    -- ^ Map from constructor (or projection) names to their arity
    --   and the case subtree.  (Projections have arity 0.)
  , etaBranch      :: Maybe (ConHead, WithArity c)
    -- ^ Eta-expand with the given (eta record) constructor. If this is
    --   present, there should not be any conBranches or litBranches.
  , litBranches    :: Map Literal c
    -- ^ Map from literal to case subtree.
  , catchAllBranch :: Maybe c
    -- ^ (Possibly additional) catch-all clause.
  , fallThrough :: Maybe Bool
    -- ^ (if True) In case of non-canonical argument use catchAllBranch.
  , lazyMatch :: Bool
    -- ^ Lazy pattern match. Requires single (non-copattern) branch with no lit
    --   branches and no catch-all.
  }
  deriving (Data, Functor, Foldable, Traversable, Show, Generic)


newtype Arg a = Arg a

-- | Case tree with bodies.

data CompiledClauses' a
  = Case (Arg Int) (Case (CompiledClauses' a))
    -- ^ @Case n bs@ stands for a match on the @n@-th argument
    -- (counting from zero) with @bs@ as the case branches.
    -- If the @n@-th argument is a projection, we have only 'conBranches'
    -- with arity 0.
  | Done [Arg NameSymbol.T] a
    -- ^ @Done xs b@ stands for the body @b@ where the @xs@ contains hiding
    --   and name suggestions for the free variables. This is needed to build
    --   lambdas on the right hand side for partial applications which can
    --   still reduce.
  | Fail [Arg NameSymbol.T]
    -- ^ Absurd case. Add the free variables here as well so we can build correct
    --   number of lambdas for strict backends. (#4280)
  deriving (Data, Functor, Traversable, Foldable, Show, Generic)

-- type CompiledClauses = CompiledClauses' Term

-- litCase :: IR.Prim -> c -> Case c
-- litCase l x = Branches False Map.empty Nothing (Map.singleton l x) Nothing (Just False) False

-- conCase :: NameSymbol.T -> Bool -> WithArity c -> Case c
-- conCase c b x = Branches False (Map.singleton c x) Nothing Map.empty Nothing (Just b) False

-- etaCase :: ConHead -> WithArity c -> Case c
-- etaCase c x = Branches False Map.empty (Just (c, x)) Map.empty Nothing (Just False) True

-- projCase :: QName -> c -> Case c
-- projCase c x = Branches True (Map.singleton c $ WithArity 0 x) Nothing Map.empty Nothing (Just False) False

-- catchAll :: c -> Case c
-- catchAll x = Branches False Map.empty Nothing Map.empty (Just x) (Just True) False


-- Elaboration of an lhs problem to a well-typed case tree is defined by the judgements:

-- DONE applies when the first user clause in P has no more copatterns and all its constraints are solved according to 􏰄; 􏰁 ⊢ E ⇒ SOLVED(σ ). If this is the case, then construction of the case tree is finished, adding the clause clause 􏰁 ⊢ f q ̄ 􏰂→ vσ : C to the signature.

-- INTRO applies when C is a function type and all the user clauses have at least one application copattern. It constructs the case tree λx. Q, using P (x : A) to construct the subtree Q.