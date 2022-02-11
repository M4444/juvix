-- | ShowReferences provides the Context a more readable Record
-- datastructure. Thus when one tries to show the Record Data structure
-- inside of =Context.Definition=, we don't get #<stm-map> but instead
-- the full symbol qualifying map.
module Juvix.Test.Data.Context.ShowReferences
  ( stmRecordToShowRecord,
    defToShowDef,
    ShowRecord (..),
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

data Info term ty sumRep = Info
  { infoTable :: HashMap.T Symbol Sexp.T,
    infoDef :: Definition term ty sumRep
  }
  deriving (Show, Read, Generic, Eq)

-- | Definition acts like Types.Record, except that the Record type
-- with an STM map is replaced by a showable variant
data Definition term ty sumRep
  = Def (Types.Def term ty)
  | Record (ShowRecord term ty sumRep)
  | TypeDeclar
      { definitionRepr :: sumRep
      }
  | Unknown
      { definitionMTy :: Maybe ty
      }
  | Information
      { definitionInfo :: [Types.Information]
      }
  | CurrentNameSpace
  | SumCon (Types.SumT term ty)
  deriving (Show, Read, Generic, Eq)

data ShowRecord term ty sumRep = ShowRec
  { contents :: NameSpace.T (Info term ty sumRep),
    mTy :: Maybe ty,
    openList :: [Open.TName NameSymbol.T],
    qualifiedMap :: ShowSymbolMap
  }
  deriving (Show, Read, Generic, Eq)

stmRecordToShowRecord ::
  Types.Record term ty sumRep -> IO (ShowRecord term ty sumRep)
stmRecordToShowRecord record = do
  newContents <- traverse defToShowDef (Types.recordContents record)
  newMap <- stmMapToHashMap (Types.recordQualifiedMap record)
  pure
    ShowRec
      { contents = newContents,
        mTy = Types.recordMTy record,
        openList = Types.recordOpenList record,
        qualifiedMap = newMap
      }

defToShowDef ::
  Types.Info term ty sumRep -> IO (Info term ty sumRep)
defToShowDef (Types.Info m (Types.Def definition)) =
  Def definition |> Info m |> pure
defToShowDef (Types.Info m (Types.SumCon sumcons)) =
  SumCon sumcons |> Info m |> pure
defToShowDef (Types.Info m (Types.Unknown unknow)) =
  Unknown unknow |> Info m |> pure
defToShowDef (Types.Info m (Types.TypeDeclar typ)) =
  TypeDeclar typ |> Info m |> pure
defToShowDef (Types.Info m (Types.Information fo)) =
  Information fo |> Info m |> pure
defToShowDef (Types.Info m Types.CurrentNameSpace) =
  CurrentNameSpace |> Info m |> pure
defToShowDef (Types.Info m (Types.Record d)) =
  stmRecordToShowRecord d >>| Record >>| Info m

stmMapToHashMap ::
  (Eq key, Hashable key) => STM.Map key value -> IO (HashMap.T key value)
stmMapToHashMap map =
  STM.listT map
    |> ListT.toList
    |> STM.atomically
    >>| HashMap.fromList
