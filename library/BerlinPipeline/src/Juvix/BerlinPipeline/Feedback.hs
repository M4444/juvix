module Juvix.BerlinPipeline.Feedback
  ( -- ** Type API
    T (..),
    Message (..),
    Level (..),
    Eff,

    -- ** Indexing Into the Feedback Messages
    messageAt,
    contentsAt,
    findMessageAt,
    findContentsAt,
  )
where

import Juvix.Library
import qualified Juvix.Sexp as Sexp

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------

data T =
  -- TODO ∷ if Messages have an idx... we should really use a
  -- hashtable rather than a list...
  T {messages :: [Message], currentId :: Integer}
  deriving (Show)

data Message = Message
  { level :: Level,
    -- contents are a sexp as we wish to serialize the structure.
    contents :: Sexp.T,
    identifier :: Integer
  }
  deriving (Show)

type Eff m = HasState "feedback" T m

-- this should really be an open type for different kinds of
-- conditions that one may signal.
data Level = Warning | Error | Note
  deriving (Show)

--------------------------------------------------------------------------------
-- Effectful functions
--------------------------------------------------------------------------------

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
