module Juvix.Core.CaseTree where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.Base.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import Debug.Pretty.Simple 

-- Inputs of the algorithm

-- 1. A signature Sigma containing previous declarations, as well as clauses fot the branches of the case tree that have already been checked
-- 2. A context T containing the types of the pattern variables: dom(T) = PV(q)
-- 3. The function f currently being checked
-- 4. The copatterns q for the current branch of the case tree
-- 5. The refined target type C of the current branch
-- 6. The user input P, which is described below

-- The outputs of the algorithm are a signature Sigma' extending Sigma with new clauses and a well-typed case tree Q such that `Simga; T |- f q := Q : C ~> Sigma'`

-- We represent the user input P to the algorithm as an ordered list of partially decomposed clauses, called a left-hand side problem. Each partially decomposed claused is of the form:
-- [E]q -> rhs, where E is an (unordered) set of constraints

clausesToCaseTree ::
  forall primTy primVal.
  (Show primTy, Show primVal) =>
  Core.RawFunction IR.T primTy primVal ->
  Core.RawFunctionCase IR.T primTy primVal
clausesToCaseTree (Core.RawFunction name usage ty clauses) =
  pTraceShow ("Type", ty) $ Core.RawFunctionCase name usage ty (go 0 clauses Nothing)
  where
    go :: Int -> NonEmpty (Core.RawFunClause IR.T primTy primVal) -> Maybe (Core.Term IR.T primTy primVal) -> Core.CaseTree IR.T primTy primVal
    go idx clauses bodyM =
      let nextArgPats = nextArgPatterns clauses
          nextClauses = elimNextArgPatterns clauses
          zipped = zip [0 ..] nextArgPats
       in case (zipped, bodyM) of
            ([], Just body) -> IR.Done [] body
            ([], Nothing) -> IR.Fail []
            _ -> IR.Case (Core.Arg idx (extractSigPointer ty idx)) $ f <$> zipped
              where
                f :: (Int, Core.Pattern IR.T primTy primVal) -> Core.Branch IR.T primTy primVal
                f (idx, pat) =
                  let cl = clauses NonEmpty.!! idx
                   in IR.Branch pat (go (idx + 1) nextClauses (Just $ Core.rawClauseBody cl))

elimNextArgPatterns :: NonEmpty (Core.RawFunClause ext primTy primVal) -> NonEmpty (Core.RawFunClause ext primTy primVal)
elimNextArgPatterns (Core.RawFunClause tel (pat : pats) rhs catchAll :| clauses) =
  Core.RawFunClause tel pats rhs catchAll :| (reverse $ go clauses [])
  where
    go [] acc = acc
    go (Core.RawFunClause tel (pat : pats) rhs catchAll : clauses) acc =
      go clauses ((Core.RawFunClause tel pats rhs catchAll) : acc)
    go _ _ = panic "Invalid elimNextPattern"
elimNextArgPatterns c = c

nextArgPatterns :: NonEmpty (Core.RawFunClause ext primTy primVal) -> [Core.Pattern ext primTy primVal]
nextArgPatterns (Core.RawFunClause tel (pat : pats) rhs catchAll :| clauses) =
  go clauses [pat]
  where
    go [] acc = reverse acc
    go (Core.RawFunClause _ (pat : pats) rhs _ : clauses) acc =
      go clauses (pat : acc)
nextArgPatterns (Core.RawFunClause tel [] rhs catchAll :| clauses) =
  []

nextPatternSig :: Core.Term ext primTy primVal -> Core.Term ext primTy primVal
nextPatternSig (Core.Pi _ pat _ _) = pat
nextPatternSig _ = panic "Invalid nextPatternSig"

extractSigPointer :: IR.Term primTy primVal -> Int -> NameSymbol.T
extractSigPointer (IR.Pi usage t1 t2) 0 = fromGlobal t1
  where
    fromGlobal (IR.Elim (IR.Free (Core.Global g))) = g
    fromGlobal _ = panic "Invalid global"
extractSigPointer (IR.Pi usage t1 t2) n = extractSigPointer t2 (n - 1)
extractSigPointer _ _ = panic "Failed to extract sig pointer"
    