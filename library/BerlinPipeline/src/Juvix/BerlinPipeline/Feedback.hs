module Juvix.BerlinPipeline.Feedback
  ( -- ** Type API
    T (..),
    Message (..),
    Level (..),
    Eff,

    -- ** Giving Feedback
    warn,
    error,
    note,

    -- ** Printing Feedback
    dumpFeedback,
    formatFeedback,

    -- ** Initialize Feedback
    empty,

    -- ** Indexing Into the Feedback Messages
    messageAt,
    contentsAt,
    findMessageAt,
    findContentsAt,

    -- ** No Effectful versions of Effectful functions
    addMessageNoEff,
    getErrors,
  )
where

import Juvix.Library hiding (empty, note)
import qualified Juvix.Sexp as Sexp
import Prelude (String)

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------

data T =
  -- TODO ∷ if Messages have an idx... we should really use a
  -- hashtable rather than a list...
  T {messages :: [Message], currentId :: Integer}
  deriving (Show, Eq)

data Message = Message
  { level :: Level,
    -- contents are a sexp as we wish to serialize the structure.
    contents :: Sexp.T,
    identifier :: Integer
  }
  deriving (Show, Eq)

type Eff m = HasState "feedback" T m

-- this should really be an open type for different kinds of
-- conditions that one may signal.
data Level = Warning | Error | Note
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Initialized
--------------------------------------------------------------------------------

empty :: T
empty = T [] 0

--------------------------------------------------------------------------------
-- Effect Messages
--------------------------------------------------------------------------------

addMessage :: Eff m => Level -> Sexp.T -> m ()
addMessage level contents = do
  modify @"feedback" (addMessageNoEff level contents)

warn :: Eff m => Sexp.T -> m ()
warn = addMessage Warning

error :: Eff m => Sexp.T -> m ()
error = addMessage Error

note :: Eff m => Sexp.T -> m ()
note = addMessage Note

addMessageNoEff :: Level -> Sexp.T -> T -> T
addMessageNoEff level contents T {messages, currentId} = do
  let message = Message {level, contents, identifier = currentId}
  T {messages = message : messages, currentId = succ currentId}

--------------------------------------------------------------------------------
-- Printing Functions
--------------------------------------------------------------------------------

dumpFeedback :: T -> IO ()
dumpFeedback T {messages} = do
  formatFeedback messages |> putStrLn

formatFeedback :: [Message] -> String
formatFeedback =
  fold . intersperse "\n" . fmap showMessage . reverse
  where
    showMessage Message {level, contents, identifier} =
      show identifier <> " " <> showLevel level <> show contents
    showLevel Note = "Note: "
    showLevel Error = "ERROR: "
    showLevel Warning = "WARN: "

--------------------------------------------------------------------------------
-- Indexing Functions
--------------------------------------------------------------------------------

-- | @messageAt@ finds the given @Message@ based on the ID handed to
-- it. If the @Message@ is not there it makes a Note saying the
-- message cant be found.
messageAt :: Integer -> T -> Message
messageAt idx =
  fromMaybe (Message Note (Sexp.string note) idx) . findMessageAt idx
  where
    note = "Message with ID " <> show idx <> " is not in the Feedback"

-- | @contentsAt@ simply calls @messageAt@ and grabs the contents out
-- of it.
contentsAt :: Integer -> T -> Sexp.T
contentsAt idx = contents . messageAt idx

-- | @findMessageAt@ finds the given @Message@ based on the ID handed
-- to it.
findMessageAt :: Integer -> T -> Maybe Message
findMessageAt idx = find ((== idx) . identifier) . messages

-- | @findContentsAt@ simply calls @findMessageAt@ and grabs the
-- contents out of it.
findContentsAt :: Integer -> T -> Maybe Sexp.T
findContentsAt idx = fmap contents . findMessageAt idx

--------------------------------------------------------------------------------
-- Filtering Functions
--------------------------------------------------------------------------------

messagesByLevel :: Level -> T -> [Message]
messagesByLevel l f = filter (\m -> level m == l) (messages f)

getErrors :: T -> [Message]
getErrors = messagesByLevel Error
