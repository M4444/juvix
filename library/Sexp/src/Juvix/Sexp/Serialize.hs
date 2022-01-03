{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Juvix.Sexp.Serialize where

import qualified Data.Char8 as Char8
import GHC.Generics as Generics
-- import qualified GHC.Types as Types
import Juvix.Library hiding (foldr)
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Sexp.Types as Sexp hiding (double)

class Serialize a where
  serialize :: a -> T
  default serialize :: (Generic a, GSerializeOptions (Rep a)) => a -> T
  serialize = serializeOpt (Options mempty)

  deserialize :: T -> Maybe a
  default deserialize :: (Generic a, GSerializeOptions (Rep a)) => T -> Maybe a
  deserialize = deserializeOpt (Options mempty)

data Options = Options
  { -- this decides if the constructors should be renamed
    constructorMapping :: Map.T [Char] NameSymbol.T
  }

class GSerializeOptions f where
  gputOpt :: Options -> f a -> T
  ggetOpt :: Options -> T -> Maybe (f a)

serializeOpt :: (GSerializeOptions (Rep a), Generic a) => Options -> a -> T
serializeOpt opt t = gputOpt opt (from t)

deserializeOpt :: (Generic b, GSerializeOptions (Rep b)) => Options -> T -> Maybe b
deserializeOpt opt t = to <$> ggetOpt opt t

gput :: (GSerializeOptions f) => f a -> T
gput = gputOpt (Options mempty)

gget :: (GSerializeOptions f) => T -> Maybe (f a)
gget = ggetOpt (Options mempty)

----------------------------------------
-- U1
----------------------------------------

instance GSerializeOptions U1 where
  gputOpt __ U1 = Nil
  ggetOpt _ _xs = Just U1

----------------------------------------
-- M1
----------------------------------------

instance (GSerializeOptions a) => GSerializeOptions (D1 i a) where
  gputOpt opt (M1 x) =
    gputOpt opt x
  ggetOpt opt xs = M1 <$> ggetOpt opt xs

-- Make an alternative version that cares about the selector
-- constructor
instance (Selector i, GSerializeOptions a) => GSerializeOptions (S1 i a) where
  gputOpt opt y@(M1 x) =
    gputOpt opt x
  ggetOpt opt xs =
    -- see if car is correct
    M1 <$> ggetOpt opt (car xs)

-- can we make consturctors with no arguments not be a list!?  for
-- example if we have @| Test@ We want it not to be (:test), but :test
instance (Constructor i, GSerializeOptions a) => GSerializeOptions (C1 i a) where
  gputOpt opt y@(M1 x) =
    let name =
          case constructorMapping opt Map.!? (conName y) of
            Nothing ->
              mlNameToReservedLispName (conName y)
            Just name ->
              name
     in case gputOpt opt x of
          Sexp.Nil -> Sexp.Atom (A name Nothing)
          otherwis -> Cons (Sexp.Atom (A name Nothing)) otherwis
  ggetOpt opt xs =
    case xs of
      Cons (Atom (A nameOf _)) _ -> logic nameOf
      Atom (A nameOfSingleCon _) -> logic nameOfSingleCon
      __________________________ -> Nothing
    where
      -- we need to cdr past the argument
      maybeX = ggetOpt opt (cdr xs)
      logic name1 =
        case maybeX of
          Just t ->
            let m1 = M1 t
                name =
                  case constructorMapping opt Map.!? (conName m1) of
                    Nothing ->
                      mlNameToReservedLispName (conName m1)
                    Just name ->
                      name
             in if
                    | name1 == name -> Just m1
                    | otherwise -> Nothing
          Nothing -> Nothing

----------------------------------------
-- Sum
----------------------------------------

instance (GSerializeOptions a, GSerializeOptions b) => GSerializeOptions (a :+: b) where
  gputOpt opt (L1 x) = gputOpt opt x
  gputOpt opt (R1 x) = gputOpt opt x

  -- is this correct?
  ggetOpt opt xs = (L1 <$> ggetOpt opt xs) <|> (R1 <$> ggetOpt opt xs)

instance (GSerializeOptions a, GSerializeOptions b) => GSerializeOptions (a :*: b) where
  gputOpt opt (a :*: b) = append (gputOpt opt a) (gputOpt opt b)

  -- is this correct also!?
  ggetOpt opt xs = (:*:) <$> ggetOpt opt xs <*> ggetOpt opt (cdr xs)

----------------------------------------
-- K1
----------------------------------------

instance Serialize a => GSerializeOptions (K1 i a) where
  gputOpt _ (K1 x) = Sexp.Cons (serialize x) Nil
  ggetOpt _ xs = K1 <$> deserialize xs

mlNameToReservedLispName :: [Char] -> NameSymbol.T
mlNameToReservedLispName [] = NameSymbol.fromString ""
mlNameToReservedLispName (x : xs) =
  let upperToDash x
        | Char8.isUpper x = ['-', Char8.toLower x]
        | otherwise = [x]
      properName =
        xs >>= upperToDash
   in NameSymbol.fromString (':' : Char8.toLower x : properName)

----------------------------------------
-- Base Instances
----------------------------------------

instance Serialize a => Serialize (Base a) where
  serialize (Sexp.Cons x xs) = Sexp.Cons (serialize x) (serialize xs)
  serialize Sexp.Nil = Sexp.Nil
  serialize (Sexp.Atom a) = serialize a

  deserialize sexp@(Sexp.Cons x xs) =
    case deserialize sexp :: Serialize a => Maybe a of
      Just ps -> Just (Atom (P ps Nothing))
      Nothing -> Sexp.Cons <$> deserialize x <*> deserialize xs
  deserialize Sexp.Nil = Just Sexp.Nil
  deserialize (Atom i) =
    Atom
      <$>
      -- why can't I dispatch to the atom call?
      case i of
        A n i -> Just $ A n i
        N n i -> Just $ N n i
        D n i -> Just $ D n i
        S n i -> Just $ S n i
        P () _ -> Nothing

instance Serialize a => Serialize (Atom a) where
  serialize i =
    case i of
      A n i -> Atom $ A n i
      N n i -> Atom $ N n i
      D n i -> Atom $ D n i
      S n i -> Atom $ S n i
      P n _ -> serialize n
  deserialize (Atom i) =
    case i of
      A n i -> Just $ A n i
      N n i -> Just $ N n i
      D n i -> Just $ D n i
      S n i -> Just $ S n i
      P () _ -> Nothing
  deserialize _ = Nothing

instance Serialize a => Serialize [a] where
  serialize xs = foldr' (Cons . serialize) Sexp.Nil xs
  deserialize c@Cons {} =
    foldr (\x xs -> flip (:) <$> xs <*> deserialize x) (Just []) c
  deserialize Nil = Just []
  deserialize _ = Nothing

instance Serialize Text where
  serialize i = Atom (S i Nothing)
  deserialize (Atom (S i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize Double where
  serialize i = Atom (D i Nothing)
  deserialize (Atom (D i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize Symbol where
  serialize i = Atom (A (NameSymbol.fromSym i) Nothing)
  deserialize (Atom (A i Nothing)) = Just (NameSymbol.toSym i)
  deserialize _ = Nothing

instance Serialize NameSymbol.T where
  serialize i = Atom (A i Nothing)
  deserialize (Atom (A i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize Integer where
  serialize i = Atom (N i Nothing)
  deserialize (Atom (N i Nothing)) = Just i
  deserialize _ = Nothing

instance Serialize () where
  serialize () = Nil
  deserialize Nil = Just ()
  deserialize _ = Nothing

----------------------------------------
-- Sexp helper functions
----------------------------------------

foldr :: (Base a -> p -> p) -> p -> Base a -> p
foldr f acc ts =
  case ts of
    Cons a as -> f a (foldr f acc as)
    Atom ____ -> f ts acc
    Nil -> acc

append :: Base a -> Base a -> Base a
append xs ys = foldr Cons ys xs

-- | @car@ grabs the head of the list
car :: Base a -> Base a
car (Cons x _) = x
car Nil = Nil
car (Atom a) = Atom a

-- | @cdr@ grabs the tail of the list
cdr :: Base a -> Base a
cdr (Cons _ xs) = xs
cdr Nil = Nil
cdr (Atom a) = Atom a
