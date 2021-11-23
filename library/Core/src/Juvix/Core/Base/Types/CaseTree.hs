module Juvix.Core.Base.Types.CaseTree where

import qualified Data.Map as Map
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Data.List.NonEmpty as NonEmpty

data WithArity c = WithArity { arity :: Int, content :: c }
  deriving (Data, Functor, Foldable, Traversable, Show, Generic)

-- -- | Branches in a case tree.

data Branches c = Branches
  { projPatterns   :: Bool
    -- ^ We are constructing a record here (copatterns).
    --   'conBranches' lists projections.
  , conBranches    :: Map NameSymbol.T (WithArity c)
    -- ^ Map from constructor (or projection) names to their arity
    --   and the case subtree.  (Projections have arity 0.)
  , litBranches    :: Map Text c
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

data CaseTree a
  = Case (Arg Int) (Branches (CaseTree a))
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
    --   number of lambdas for strict backends. 
  deriving (Data, Functor, Traversable, Foldable, Show, Generic)


-- Elaboration of an lhs problem to a well-typed case tree is defined by the judgements:

-- DONE applies when the first user clause in P has no more copatterns and all its constraints are solved according to 􏰄; 􏰁 ⊢ E ⇒ SOLVED(σ ). If this is the case, then construction of the case tree is finished, adding the clause clause 􏰁 ⊢ f q ̄ 􏰂→ vσ : C to the signature.

-- INTRO applies when C is a function type and all the user clauses have at least one application copattern. It constructs the case tree λx. Q, using P (x : A) to construct the subtree Q.