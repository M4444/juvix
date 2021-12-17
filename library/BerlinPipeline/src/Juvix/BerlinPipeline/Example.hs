module Juvix.BerlinPipeline.Example where

import Control.Lens hiding ((|>))
import qualified Data.Set as Set
import Juvix.Library
import qualified Juvix.BerlinPipeline.Step as Step
import qualified Juvix.Sexp as Sexp
import Juvix.Sexp.Structure.Lens
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure
import qualified Juvix.BerlinPipeline.Meta as Meta
import qualified Juvix.Library.Trace as Trace
import qualified Juvix.BerlinPipeline.Automation as Automation
import Prelude (error)


data MinimalEnv =
  Env { trace ∷ Meta.Trace
      , feedback :: Meta.Feedback
      }
      deriving (Show, Generic)

type MinimalAlias = State MinimalEnv

newtype MinimalM a = Ctx {_run :: MinimalAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
   ( HasSink "trace" Meta.Trace,
     HasSource "trace" Meta.Trace,
     HasState "trace" Meta.Trace
   )
   via StateField "trace" MinimalAlias
  deriving
   ( HasSink "feedback" Meta.Feedback,
     HasSource "feedback" Meta.Feedback,
     HasState "feedback" Meta.Feedback
   )
   via StateField "feedback" MinimalAlias


instance Automation.HasExtract MinimalM Identity where
  extract = undefined

condPass =
  Step.namePass
    (undefined Automation.runSimplifiedPass) -- condTrans
    "Desugar.cond-to-if"

condTrans
  :: (Automation.HasCurrent s Sexp.T, HasState "trace" Trace.T m) => s -> m Automation.Job
condTrans simplify = do
  Trace.withScope "Desugar.cond-transformation" [show (simplify ^. Automation.current)] $
    condTransform (simplify ^. Automation.current)
    |> (\transformed -> Automation.ProcessJobNoEnv transformed [])
    |> Automation.PJob
    |> pure


-- | @condTransform@ - CondTransform turns the cond form of the fronted
-- language into a series of ifs
-- - BNF input form:
--   + (:cond (pred-1 result-1) … (pred-n result-n))
-- - BNF output form:
--   + (if pred-1 result-1 (if pred-2 result-2 (… (if pred-n result-n))))
condTransform :: Sexp.T -> Sexp.T
condTransform xs = Sexp.foldPred xs (== Structure.nameCond) condToIf
  where
    condToIf atom cdr
      | Just cond <- Structure.toCond (Sexp.Atom atom Sexp.:> cdr),
        Just last <- lastMay (cond ^. entailments) =
        let acc =
              Structure.IfNoElse (last ^. predicate) (last ^. answer)
                |> Structure.fromIfNoElse
         in foldr generation acc (initSafe (cond ^. entailments))
              |> Sexp.addMetaToCar atom
      | otherwise = error "malformed cond"
    --
    generation predAns acc =
      Structure.If (predAns ^. predicate) (predAns ^. answer) acc
        |> Structure.fromIf
