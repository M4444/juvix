{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Juvix.Core.Base.Types.CaseTree where

import qualified Data.Map as Map
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Aeson as A


-- data WithArity c = WithArity { arity :: Int, content :: c }
--   deriving (Data, Eq, Functor, Foldable, Traversable, Show, Generic, NFData)

-- instance (A.ToJSON a) => A.ToJSON (WithArity a) where
--   toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- instance (A.FromJSON a) => A.FromJSON (WithArity a) where
--   parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- -- | Store the names of the record fields in the constructor.
-- --   This allows reduction of projection redexes outside of TCM.
-- --   For instance, during substitution and application.
-- data ConHead = ConHead
--   { conName       :: QName         -- ^ The name of the constructor.
--   , conDataRecord :: DataOrRecord  -- ^ Data or record constructor?
--   , conInductive  :: Induction     -- ^ Record constructors can be coinductive.
--   , conFields     :: [Arg QName]   -- ^ The name of the record fields.
--       --   'Arg' is stored since the info in the constructor args
--       --   might not be accurate because of subtyping (issue #2170).
--   } deriving (Data, Show, Generic)


-- | Branches in a case tree.

-- data Branches c = Branches
--   { projPatterns   :: Bool
--     -- ^ We are constructing a record here (copatterns).
--     --   'conBranches' lists projections.
--   , conBranches    :: Map NameSymbol.T (WithArity c)
--     -- ^ Map from constructor (or projection) names to their arity
--     --   and the case subtree.  (Projections have arity 0.)
--   , etaBranch      :: Maybe (ConHead, WithArity c)
--     -- ^ Eta-expand with the given (eta record) constructor. If this is
--     --   present, there should not be any conBranches or litBranches.
--   , litBranches    :: Map Text c
--     -- ^ Map from literal to case subtree.
--   , catchAllBranch :: Maybe c
--     -- ^ (Possibly additional) catch-all clause.
--   , fallThrough :: Maybe Bool
--     -- ^ (if True) In case of non-canonical argument use catchAllBranch.
--   , lazyMatch :: Bool
--     -- ^ Lazy pattern match. Requires single (non-copattern) branch with no lit
--     --   branches and no catch-all.
--   }
--   deriving (Data, Eq, Functor, Foldable, Traversable, Show, Generic, NFData)

-- instance (A.ToJSON a) => A.ToJSON (Branches a) where
--   toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- instance (A.FromJSON a) => A.FromJSON (Branches a) where
--   parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- data ArgMeta ext ty val = ArgMeta
--   { sig :: Maybe (Core.Term ext ty val)}

-- data Arg a = Arg 
--   { unarg :: a
--   -- , meta :: ArgMeta ext ty val
--   } deriving (Data, Eq, Show, Generic, NFData)

-- instance (A.ToJSON a) => A.ToJSON (Arg a) where
--   toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- instance (A.FromJSON a) => A.FromJSON (Arg a) where
--   parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- -- | Case tree with bodies.

-- -- data CaseTree a
-- --   = Case (Arg Int) (Branches (CaseTree a))
-- --     -- ^ @Case n bs@ stands for a match on the @n@-th argument
-- --     -- (counting from zero) with @bs@ as the case branches.
-- --     -- If the @n@-th argument is a projection, we have only 'conBranches'
-- --     -- with arity 0.
-- --   | Done [Arg NameSymbol.T] a
-- --     -- ^ @Done xs b@ stands for the body @b@ where the @xs@ contains hiding
-- --     --   and name suggestions for the free variables. This is needed to build
-- --     --   lambdas on the right hand side for partial applications which can
-- --     --   still reduce.
-- --   | Fail [Arg NameSymbol.T]
-- --     -- ^ Absurd case. Add the free variables here as well so we can build correct
-- --     --   number of lambdas for strict backends. 
-- --   deriving (Data, Eq, Functor, Traversable, Foldable, Show, Generic, NFData)

-- -- instance (A.ToJSON a) => A.ToJSON (CaseTree a) where
-- --   toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- -- instance (A.FromJSON a) => A.FromJSON (CaseTree a) where
-- --   parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- -- Elaboration of an lhs problem to a well-typed case tree is defined by the judgements:

-- -- DONE applies when the first user clause in P has no more copatterns and all its constraints are solved according to 􏰄; 􏰁 ⊢ E ⇒ SOLVED(σ ). If this is the case, then construction of the case tree is finished, adding the clause clause 􏰁 ⊢ f q ̄ 􏰂→ vσ : C to the signature.

-- -- INTRO applies when C is a function type and all the user clauses have at least one application copattern. It constructs the case tree λx. Q, using P (x : A) to construct the subtree Q.

-- | Branches in a case tree.

-- data Pat = Pat

-- data Branches c = Branches [(Pat, c)]

-- -- | Case tree with bodies.

-- data CaseTree a
--   = Case (Arg Int) (Branches (CaseTree a))
--     -- ^ @Case n bs@ stands for a match on the @n@-th argument
--     -- (counting from zero) with @bs@ as the case branches.
--     -- If the @n@-th argument is a projection, we have only 'conBranches'
--     -- with arity 0.
--   | Done [Arg NameSymbol.T] a
--     -- ^ @Done xs b@ stands for the body @b@ where the @xs@ contains hiding
--     --   and name suggestions for the free variables. This is needed to build
--     --   lambdas on the right hand side for partial applications which can
--     --   still reduce.
--   | Fail [Arg NameSymbol.T]
--     -- ^ Absurd case. Add the free variables here as well so we can build correct
--     --   number of lambdas for strict backends. 
--   deriving (Data, Eq, Functor, Traversable, Foldable, Show, Generic, NFData)

-- data D (m : nat) : Set where
--    c : (n: N) -> D m

-- foo : (m : nat) -> D (suc m) -> nat
-- foo = Lam (Case (Arg 0) (Branches
--             { (Pattern "X.c" [Pattern (PatternName "n")]) -> Case (Bound 0) 
--                            (Branches { Pattern "Nat.suc" [Pattern (PatternName "k")] 
--                                                -> Done [] (Elim (App ((+) (Elim (Bound 2)) (Elim (Bound 0))))),
--                                        
--                                      }) 
--             }))

-- max : nat -> nat -> nat
-- Case (Arg 0) (Branches
--          { (Pattern "Nat.zero" [])) -> Done [] (Lambda (Elim (Bound 0))
--          , (Pattern "Nat.succ" [Pattern (PatternName "p")] -> Case (Arg 1) 
--                                  Branches { (Pattern "Nat.zero" [])) 
--                                                      -> Done [] (Con (Elim (Bound 0)))
--                                            , (Pattern "Nat.succ" [Pattern (PatternName "q")]
--                                                      -> Done [] (Elim (App Any (clausesCaseTree (lookupFun "ModX.max") (Elim (Bound 1)))) (Elim (Bound 0))})


-- RawFunction { ..., clauses: NonEmpty [RawFunctionClause]}
-- RawFunction' { ..., caseTree: CaseTree {..}}

-- max : nat -> nat -> nat

-- "Nat.zero": 
-- Pattern "" [Pattern]

-- Lambda [Pattern (PatternData "Zero" []), Pattern ()] (Lambda ...)

-- lookupGlobal : NameSymbol.T -> Term ext ty val

-- max : nat -> nat -> nat
-- Case (Arg 0 (ArgMeta ...)) (Branches
--          { (Pattern "Nat.zero" [])) -> Done [] (Lambda (Elim (Bound 0))
--          , (Pattern "Nat.succ" [Pattern (PatternName "p")] -> Case (Arg 1) 
--                                  Branches { (Pattern "Nat.zero" [])) 
--                                                      -> Done [] (Con (Elim (Bound 0)))
--                                            , (Pattern "Nat.succ" [Pattern (PatternName "q")]
--                                                      -> Done [] (Elim (App Any (toCaseTree (lookupFun) (Elim (Bound 1)))) (Elim (Bound 0))})

--          }

-- Case (Arg 0) (Branches {"Nat.zero" -> (WithArity 0 (Case (Arg 1) ())
--                         "Nat.suc" -> (WithArity 1 (Case (Arg 1) 
--                                         (Branches {"Nat.zero" -> 
--                                                   })))
--                        }


-- Case (Arg 0) (Branches {"Nat.zero" -> (WithArity 0 (Case (Arg 1) ())
--                         "Nat.suc" -> (WithArity 1 (Case (Arg 1) 
--                                         (Branches {"Nat.zero" -> 
--                                                   })))
--                        }
