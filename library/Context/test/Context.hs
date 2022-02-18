module Context where

import Control.Lens (over, (^.), (^?), _Just)
import qualified Juvix.Context as Context
import qualified Juvix.Context.NameSpace as NameSpace
import Juvix.Library
import qualified Juvix.Library.HashMap as HashMap
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import qualified System.IO.Unsafe as Unsafe
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

foo :: Context.T
foo = Unsafe.unsafePerformIO (Context.empty ("Foo" :| ["Bar", "Baz"]))

unsafeEmpty :: NameSymbol.T -> Context.T
unsafeEmpty x = Unsafe.unsafePerformIO (Context.empty x)

top :: T.TestTree
top =
  T.testGroup
    "Context tests:"
    [ switchAboveLookupCheck,
      switchSelf,
      checkFullyResolvedName,
      checkCorrectResolution,
      privateFromAbove,
      privateBeatsPublic,
      localBeatsGlobal,
      nonRelatedModuleStillPersists,
      emptyWorksAsExpectedSingle,
      topLevelDoesNotMessWithInnerRes,
      addRemoveDoesNotAdd
    ]

makeTm :: Sexp.Serialize a => a -> Context.Definition
makeTm = Context.Term . Sexp.serialize

switchAboveLookupCheck :: T.TestTree
switchAboveLookupCheck =
  T.testCase
    "switch to module above and lookup value from below"
    ( do
        let added = Context.add (NameSpace.Pub "a") (makeTm @Integer 3 |> Context.Info mempty) foo
            --
            looked = Context.lookup (pure "a") added
        Right switched <- Context.switchNameSpace ("Foo" :| ["Bar"]) added
        --
        let looked' = Context.lookup ("Baz" :| ["a"]) switched
        looked T.@=? looked'
    )

-- should we allow a switch to itself... should we just make it ID?
switchSelf :: T.TestTree
switchSelf =
  T.testCase
    "switching namespace to self is left"
    ( do
        swtiched <- Context.switchNameSpace ("Foo" :| ["Bar", "Baz"]) foo
        swtiched T.@=? Right foo
    )

checkFullyResolvedName :: T.TestTree
checkFullyResolvedName =
  T.testCase
    "relative lookup is the same as fully qualified"
    ( do
        Right relative <-
          Context.switchNameSpace (pure "Barry") foo
        --
        Right fullQual <-
          Context.switchNameSpace ("Foo" :| ["Bar", "Baz", "Barry"]) foo
        --
        Context.currentName relative T.@=? Context.currentName fullQual
    )

-- this test checks that the local variable is added and the global
checkCorrectResolution :: T.TestTree
checkCorrectResolution =
  T.testGroup
    "correct resolution test"
    [ T.testCase
        "topLevel value same as local: "
        ( do
            (outside, current) <- run
            Context.extractValue outside T.@=? Context.extractValue current
        ),
      T.testCase
        "topLevel is outside: "
        ( do
            (outside, _current) <- run
            isOutside outside T.@=? True
        ),
      T.testCase
        "current is local: "
        ( do
            (_, current) <- run
            isCurrent current T.@=? True
        )
    ]
  where
    isOutside from =
      from ^. Context.nameSpace == Context.Outside
    isCurrent from =
      from ^. Context.nameSpace /= Context.Outside
    run = do
      Right inner <- Context.switchNameSpace (pure "Gkar") foo
      --
      let added =
            Context.add
              (NameSpace.Pub "londo")
              (makeTm @Integer 3 |> Context.Info mempty)
              inner
      --
      Right topGkar <-
        Context.switchNameSpace (Context.topLevelName :| ["Gkar"]) added
      --
      let addedTop =
            Context.add
              (NameSpace.Pub "londo")
              (makeTm @Integer 3 |> Context.Info mempty)
              topGkar
      --
      Right switchBack <-
        Context.switchNameSpace ("Foo" :| ["Bar", "Baz"]) addedTop
      --
      let Just outside =
            switchBack Context.!? (Context.topLevelName :| ["Gkar", "londo"])
          --
          Just current = switchBack Context.!? ("Gkar" :| ["londo"])
      pure (outside, current)

privateFromAbove :: T.TestTree
privateFromAbove =
  T.testCase
    "Can't access private var from above"
    $ do
      empt <-
        Context.empty ("Ambassador" :| ["Kosh", "Vorlons"]) ::
          IO (Context.T)
      --
      let added =
            Context.add
              (NameSpace.Priv "too-late")
              ( makeTm @Text
                  "The avalanche has already started; It is too late for the pebbles to vote."
                  |> Context.Info mempty
              )
              empt
      --
      Right switched <- Context.switchNameSpace ("Ambassador" :| ["Kosh"]) added
      --
      let looked = switched Context.!? ("Vorlons" :| ["too-late"])
      isNothing looked T.@=? True

privateBeatsPublic :: T.TestTree
privateBeatsPublic =
  let empt :: Context.T
      empt = unsafeEmpty ("Londo" :| ["Mollari", "Centauri"])
      --
      added =
        Context.add
          (NameSpace.Priv "joy")
          ( makeTm @Text
              "What do you want, you moon-faced assassin of joy?"
              |> Context.Info mempty
          )
          empt
      added2 =
        Context.add
          (NameSpace.Pub "joy")
          ( makeTm @Text
              "Now, I go to spread happiness to the rest of the station. \
              \ It is a terrible responsibility but I have learned to live with it."
              |> Context.Info mempty
          )
          added
      looked = added2 Context.!? pure "joy"
   in "What do you want, you moon-faced assassin of joy?"
        |> makeTm @Text
        |> Context.Info mempty
        |> Just
        |> ((looked ^? _Just . Context.term) T.@=?)
        |> T.testCase "Can't access private var from above"

localBeatsGlobal :: T.TestTree
localBeatsGlobal =
  let empt :: Context.T
      empt = unsafeEmpty ("GKar" :| ["Narn"])
      --
      added =
        Context.add
          (NameSpace.Priv "cost")
          ( makeTm @Text
              "I have seen what power does, and I have seen what power costs. \
              \ The one is never equal to the other."
              |> Context.Info mempty
          )
          empt
      added2 =
        Context.addGlobal
          (Context.topLevelName :| ["cost"])
          ( makeTm @Text
              "I'm delirious with joy. It proves that if you confront the universe \
              \ with good intentions in your heart, it will reflect that and reward \
              \ your intent. Usually. It just doesn't always do it in the way you expect."
              |> Context.Info mempty
          )
          added
      looked = added2 Context.!? pure "cost"
   in "I have seen what power does, and I have seen what power costs. \
      \ The one is never equal to the other."
        |> makeTm @Text
        |> Context.Info mempty
        |> Just
        |> ((looked ^? _Just . Context.term) T.@=?)
        |> T.testCase "public beats global"

addRemoveDoesNotAdd :: T.TestTree
addRemoveDoesNotAdd =
  let empt :: Context.T
      empt = unsafeEmpty ("GKar" :| ["Narn"])
      --
      added =
        Context.addGlobal "zazz" (Context.Info mempty (Context.Term (Sexp.List []))) empt
      removed =
        Context.removeGlobal "zazz" added
      looked = removed Context.!? pure "zazz"
   in T.testCase "adding then removal removes" (Nothing T.@=? looked)



nonRelatedModuleStillPersists :: T.TestTree
nonRelatedModuleStillPersists =
  T.testCase
    "differnet module persists through switch"
    ( do
        Right topBar <- Context.switchNameSpace ("TopLevel" :| ["Bar"]) foo
        Right food <- Context.switchNameSpace ("TopLevel" :| ["Foo"]) topBar
        --
        let looked = food Context.!? (Context.topLevelName :| ["Bar"])
            --
            isMod (Context.From {fromTerm = Context.Info _ Context.Module {}}) =
              True
            isMod _ =
              False
            isOutSideRec (Just from) =
              from ^. Context.nameSpace == Context.Outside && isMod from
            isOutSideRec Nothing = False
        --
        isOutSideRec looked T.@=? True
    )

emptyWorksAsExpectedSingle :: T.TestTree
emptyWorksAsExpectedSingle =
  T.testCase
    "empty properly adds a top level module as expected:"
    $ do
      created <- Context.empty (pure "Mr-Morden") :: IO Context.T
      empt <- do
        contents <- atomically Context.emptyRecord
        pure $
          Context.T
            (Context.InfoRecord mempty contents)
            (pure "Mr-Morden")
            (HashMap.fromList [("Mr-Morden", Context.Info mempty Context.CurrentNameSpace)])
            HashMap.empty
      created T.@=? empt

topLevelDoesNotMessWithInnerRes :: T.TestTree
topLevelDoesNotMessWithInnerRes =
  T.testCase
    "TopLevelname does not prohibit inner module change"
    $ do
      let created :: Context.T
          created = unsafeEmpty (pure "Shadows")
      inner <-
        Context.switchNameSpace
          (Context.topLevelName :| ["Shadows", "Mr-Morden"])
          created
      inner2 <-
        Context.switchNameSpace
          ("Shadows" :| ["Mr-Morden"])
          created
      empt <- do
        emptyRecord <- atomically Context.emptyRecord
        emptyNameSpace <- atomically Context.emptyRecord >>| Context.InfoRecord mempty
        emptyRecord
          |> over
            Context.contents
            (NameSpace.insert (NameSpace.Pub "Mr-Morden") (Context.Info mempty Context.CurrentNameSpace))
          |> Context.Module
          |> Context.Info mempty
          |> (\record -> HashMap.fromList [("Shadows", record)])
          |> (\x -> Context.T emptyNameSpace ("Shadows" :| ["Mr-Morden"]) x HashMap.empty)
          |> pure
      inner == Right empt && inner == inner2 T.@=? True
