{-# LANGUAGE RecordWildCards #-}

module Juvix.Sexp.Structure.EffectHandlerHelpers where

import Juvix.Library hiding (Handler)
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Library.NameSymbol as NameSymbol
import Control.Lens hiding ((|>))

data Effect = Effect
  { effectName :: Sexp.T,
    effectOps :: Sexp.T
  }
  deriving (Show)

data DefHandler = DefHandler
  { defHandlerName :: Sexp.T,
    defHandlerOps :: Sexp.T
  }
  deriving (Show)

data LetHandler = LetHandler
  { letHandlerName :: Sexp.T,
    letHandlerOps  :: Sexp.T,
    letHandlerRet  :: Sexp.T
  }
  deriving (Show)

data LetOp = LetOp
  { letOpName :: Sexp.T,
    letOpArgs :: Sexp.T,
    letOpBody :: Sexp.T
  }
  deriving (Show)

data LetRet = LetRet
  { letRetArg :: Sexp.T,
    letRetBody :: Sexp.T
  }
  deriving (Show)

data Do = Do
  { doStatements :: Sexp.T
  }
  deriving (Show)

data DoDeep = DoDeep
  { doDeepStatements :: [DoBodyFull]
  }
  deriving (Show)

data DoBodyFull
  = WithBinder { doBodyFullName :: NameSymbol.T,
                 doBodyFullBBody :: Sexp.T
               }
  | NoBinder { doBodyFullBody :: Sexp.T }
  deriving (Show)

data DoClause = Op DoOp | Pure DoPure
  deriving (Show)

data DoOp = DoOp
  { doOpName :: Sexp.T,
    doOpArgs :: Sexp.T
  }
  deriving (Show)

data DoPure = DoPure
  { doPureArg :: Sexp.T
  }
  deriving (Show)

makeLensesWith camelCaseFields ''DefHandler
makeLensesWith camelCaseFields ''LetHandler
makeLensesWith camelCaseFields ''Effect
makeLensesWith camelCaseFields ''LetRet
makeLensesWith camelCaseFields ''LetOp
makeLensesWith camelCaseFields ''Do
makeLensesWith camelCaseFields ''DoOp
makeLensesWith camelCaseFields ''DoPure

----------------------------------------
-- LetHandler
----------------------------------------

nameLetHandler :: NameSymbol.T
nameLetHandler = ":lethandler"

isLetHandler :: Sexp.T -> Bool
isLetHandler (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetHandler
isLetHandler _ = False

toLetHandler :: Sexp.T -> Maybe LetHandler
toLetHandler form
  | isLetHandler form =
    case form of
      _nameLetHandler Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        LetHandler sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetHandler :: LetHandler -> Sexp.T
fromLetHandler (LetHandler sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameLetHandler, sexp1, Sexp.list [Sexp.atom ":ops", sexp2], sexp3]

----------------------------------------
-- Effect
----------------------------------------

nameEffect :: NameSymbol.T
nameEffect = ":defeff"

isEffect :: Sexp.T -> Bool
isEffect (Sexp.Cons form _) = Sexp.isAtomNamed form nameEffect
isEffect _ = False

toEffect :: Sexp.T -> Maybe Effect
toEffect form
  | isEffect form =
    case form of
      _nameEffect Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Effect sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromEffect :: Effect -> Sexp.T
fromEffect (Effect sexp1 sexp2) =
  Sexp.list [Sexp.atom nameEffect, sexp1, sexp2]
----------------------------------------
-- DefHandler
----------------------------------------

nameDefHandler :: NameSymbol.T
nameDefHandler = ":defhandler"

isDefHandler :: Sexp.T -> Bool
isDefHandler (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefHandler
isDefHandler _ = False

toDefHandler :: Sexp.T -> Maybe DefHandler
toDefHandler form
  | isDefHandler form =
    case form of
      _nameDefHandler Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        DefHandler sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefHandler :: DefHandler -> Sexp.T
fromDefHandler (DefHandler sexp1 sexp2) =
  Sexp.list [Sexp.atom nameDefHandler, sexp1, sexp2]

----------------------------------------
-- LetRet
----------------------------------------

nameLetRet :: NameSymbol.T
nameLetRet = ":defret"

isLetRet :: Sexp.T -> Bool
isLetRet (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetRet
isLetRet _ = False

toLetRet :: Sexp.T -> Maybe LetRet
toLetRet form
  | isLetRet form =
    case form of
      _nameLetRet Sexp.:> sexp Sexp.:> sexp1 Sexp.:>Sexp.Nil ->
        LetRet sexp sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetRet :: LetRet -> Sexp.T
fromLetRet (LetRet sexp sexp1) =
  Sexp.list [Sexp.atom nameLetRet, sexp, sexp1]

----------------------------------------
-- LetOp
----------------------------------------

nameLetOp :: NameSymbol.T
nameLetOp = ":defop"

isLetOp :: Sexp.T -> Bool
isLetOp (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetOp
isLetOp _ = False

toLetOp :: Sexp.T -> Maybe LetOp
toLetOp form
  | isLetOp form =
    case form of
      _nameLetOp Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        LetOp sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetOp :: LetOp -> Sexp.T
fromLetOp (LetOp sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameLetOp, sexp1, sexp2, sexp3]

----------------------------------------
-- Do
----------------------------------------

nameDoBody :: NameSymbol.T
nameDoBody = ":do-body"

nameDoBodyB :: NameSymbol.T
nameDoBodyB = ":do-body-bind"

nameDoOp :: NameSymbol.T
nameDoOp = ":do-op"

nameDoPure :: NameSymbol.T
nameDoPure = ":do-pure"

nameDo :: NameSymbol.T
nameDo = ":do"

isDoBody :: Sexp.T -> Bool
isDoBody (Sexp.Cons form _) = Sexp.isAtomNamed form nameDoBody
isDoBody _ = False

isDoBodyB :: Sexp.T -> Bool
isDoBodyB (Sexp.Cons form _) = Sexp.isAtomNamed form nameDoBodyB
isDoBodyB _ = False

isDoOp :: Sexp.T -> Bool
isDoOp (Sexp.Cons form _) = Sexp.isAtomNamed form nameDoOp
isDoOp _ = False

isDoPure :: Sexp.T -> Bool
isDoPure (Sexp.Cons form _) = Sexp.isAtomNamed form nameDoPure
isDoPure _ = False

isDo :: Sexp.T -> Bool
isDo (Sexp.Cons form _) = Sexp.isAtomNamed form nameDo
isDo _ = False

toDoOp :: Sexp.T -> Maybe DoOp
toDoOp form
  | isDoOp form =
    case form of
      _nameDo Sexp.:> sexp1 Sexp.:> sexp2 ->
        DoOp sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

toDoPure :: Sexp.T -> Maybe DoPure
toDoPure form
  | isDoPure form =
    case form of
      _nameDo Sexp.:> sexp1 ->
        DoPure sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

toDo :: Sexp.T -> Maybe Do
toDo form
  | isDo form =
    case form of
      _nameDo Sexp.:> sexp1 ->
        Do sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

isDoDeep :: Sexp.T -> Bool
isDoDeep (Sexp.Cons form _) = Sexp.isAtomNamed form nameDoBodyB
isDoDeep _ = False

toDoDeep :: Sexp.T -> Maybe DoDeep
toDoDeep form
  | Just do' <- toDo form =
      pure DoDeep <*> (toDoBodyFull $ Sexp.toList $ doStatements do')
  | otherwise = Nothing

toDoBodyFull :: Maybe [Sexp.T] -> Maybe [DoBodyFull]
toDoBodyFull (Just sexps) = traverse toFullBodyFull' sexps
  where toFullBodyFull' :: Sexp.T -> Maybe DoBodyFull
        toFullBodyFull' sexp
          | isDoBodyB sexp =
            case sexp of
              _ Sexp.:> name Sexp.:> body Sexp.:> Sexp.Nil->
                pure WithBinder <*> Sexp.nameFromT name <*> pure body
              _ -> Nothing
          | isDoBody sexp =
             case sexp of
              _ Sexp.:> body Sexp.:> Sexp.Nil ->
                pure $ NoBinder body
              _ -> Nothing
          | otherwise = Nothing
toDoBodyFull Nothing = Nothing

fromDoOp :: DoOp -> Sexp.T
fromDoOp (DoOp sexp1 sexp2) =
  Sexp.list [Sexp.atom nameDoOp, sexp1, sexp2]

fromDoPure :: DoPure -> Sexp.T
fromDoPure (DoPure sexp1) =
  Sexp.list [Sexp.atom nameDoPure, sexp1]

fromDo :: Do -> Sexp.T
fromDo (Do sexp1) =
  Sexp.list [Sexp.atom nameDo, sexp1]

fromDoDeep :: DoDeep -> Sexp.T
fromDoDeep (DoDeep stas) = Sexp.list (fromDoBodyFull <$> stas)

fromDoBodyFull :: DoBodyFull -> Sexp.T
fromDoBodyFull (WithBinder {..}) =
  Sexp.list [ Sexp.atom nameDoBody,
              Sexp.list [ Sexp.atom nameDoPure,
                          doBodyFullBBody
                        ]
            ]

fromDoBodyFull (NoBinder {..}) =
  Sexp.list [ Sexp.atom nameDoBody,
              doBodyFullBody
            ]

----------------------------------------
-- Non-generated structures
----------------------------------------

data Via = Via
  { viaHandler :: Sexp.T,
    viaProgram :: Sexp.T
  }
  deriving (Show)


nameVia :: NameSymbol.T
nameVia = ":via"

isVia :: Sexp.T -> Bool
isVia (Sexp.Cons form _) = Sexp.isAtomNamed form nameVia
isVia _ = False

toVia :: Sexp.T -> Maybe Via
toVia form
  | isVia form =
    case form of
      _ Sexp.:> hand Sexp.:> prog Sexp.:> Sexp.Nil ->
        Via hand prog |> Just
      _ -> Nothing
  | otherwise = Nothing

fromVia :: Via -> Sexp.T
fromVia (Via handler prog) =
  Sexp.list [Sexp.atom nameVia, handler, prog]

data Handler = Handler
 { handlerName :: Sexp.T,
   handlerRet  :: LetRet,
   handlerOps  :: [LetOp]
 }
 deriving (Show)

nameHandler :: NameSymbol.T
nameHandler = nameLetHandler

isHandler :: Sexp.T -> Bool
isHandler = isLetHandler

toHandler :: Sexp.T -> Maybe Handler
toHandler form =
  toLetHandler form >>= transform
  where
    transform :: LetHandler -> Maybe Handler
    transform hand =
      let ret_  = toLetRet (hand ^. ret)
          ops_  = toLetOps (hand ^. ops)
          name_ = (hand ^. name)
      in pure Handler <*> pure name_ <*> ret_ <*> ops_

fromHandler :: Handler -> Sexp.T
fromHandler (Handler name ret ops) =
  Sexp.list [Sexp.atom nameLetHandler, name, fromLetRet ret, Sexp.list (fromLetOp <$> ops)]

toLetOps :: Sexp.T -> Maybe [LetOp]
toLetOps form = Sexp.toList (Sexp.cdr form) >>= traverse toLetOp
