-- | ShowReferences provides the Context a more readable Record
-- datastructure. Thus when one tries to show the Record Data structure
-- inside of =Context.Definition=, we don't get #<stm-map> but instead
-- the full symbol qualifying map.
module Juvix.Test.Data.Context.ShowReferences
  ( stmRecordToShowRecord,
    defToShowDef,
    ShowModule (..),
    Definition (..),
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Lens hiding ((|>))
import qualified Juvix.Context.NameSpace as NameSpace
import qualified Juvix.Context.Open as Open
import qualified Juvix.Context.Types as Types
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import qualified ListT
import qualified StmContainers.Map as STM

type ShowSymbolMap = HashMap.T Symbol Types.SymbolInfo

-- Aren't Closed Types wonderful

data Info = Info
  { infoTable :: HashMap.T Symbol Sexp.T,
    infoDef :: Definition
  }
  deriving (Show, Read, Generic, Eq)

-- | Definition acts like Types.Record, except that the Record type
-- with an STM map is replaced by a showable variant
data Definition
  = Term Sexp.T
  | Module ShowModule
  | -- | @CurrentNameSpace@ Signifies that this path is the current
    -- module, and that we should search the currentNameSpace instead
    CurrentNameSpace
  deriving (Show, Read, Generic, Eq)

data ShowModule = ShowMod
  { contents :: NameSpace.T Info,
    openList :: [Open.TName NameSymbol.T],
    qualifiedMap :: ShowSymbolMap
  }
  deriving (Show, Read, Generic, Eq)

stmRecordToShowRecord ::
  Types.Module -> IO (ShowModule)
stmRecordToShowRecord record = do
  newContents <- traverse defToShowDef (Types.moduleContents record)
  newMap <- stmMapToHashMap (Types.moduleQualifiedMap record)
  pure
    ShowMod
      { contents = newContents,
        openList = Types.moduleOpenList record,
        qualifiedMap = newMap
      }

defToShowDef :: Types.Info -> IO Info
defToShowDef (Types.Info m (Types.Term term)) =
  Term term |> Info m |> pure
defToShowDef (Types.Info m Types.CurrentNameSpace) =
  CurrentNameSpace |> Info m |> pure
defToShowDef (Types.Info m (Types.Module d)) =
  stmRecordToShowRecord d >>| Module >>| Info m

stmMapToHashMap ::
  (Eq key, Hashable key) => STM.Map key value -> IO (HashMap.T key value)
stmMapToHashMap map =
  STM.listT map
    |> ListT.toList
    |> STM.atomically
    >>| HashMap.fromList
