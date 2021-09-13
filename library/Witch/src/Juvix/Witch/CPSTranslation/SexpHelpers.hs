module Juvix.Witch.CPSTranslation.SexpHelpers where

import Juvix.Library hiding (fst, snd, list, empty, trace)
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.EffectHandlerHelpers as Str
import qualified Juvix.Sexp.Structure as Str
import qualified Juvix.Sexp.Structure.CoreNamed as Str
import qualified Juvix.Library.NameSymbol as NameSymbol

spair :: Sexp.T
spair = var ":pair"

empty :: Sexp.T
empty = Sexp.Nil

do' :: [Str.DoBodyFull] -> Sexp.T
do' x = Str.fromDoDeep $ Str.DoDeep x

lam :: Sexp.T -> Sexp.T -> Sexp.T
lam name x = Str.from $ Str.Lam name' x
  where (Just name') = Sexp.nameFromT name

app :: Sexp.T -> Sexp.T -> Sexp.T
app x y = Str.from $ Str.App x y

letMatch :: Sexp.T -> [Str.ArgBody] -> Sexp.T -> Sexp.T
letMatch x y z = Str.from $ Str.LetMatch x y z

fun :: Sexp.T -> Sexp.T -> Sexp.T -> Sexp.T
fun x y z = Str.from $ Str.Defun x y z

pair :: Sexp.T -> Sexp.T -> Sexp.T
pair x y = Str.from $ Str.Pair x y

-- multiple argument application
app' :: Sexp.T -> [Sexp.T] -> Sexp.T
app' f args = foldr (\arg acc -> app acc arg) f args

-- multiple argument lambda
lam' :: [Sexp.T] -> Sexp.T -> Sexp.T
lam' args body = foldr (\arg acc -> lam arg acc) body (reverse args)

var :: NameSymbol.T -> Sexp.T
var x = Sexp.atom x

skip :: Sexp.T
skip = Sexp.Nil

unlist :: Sexp.T -> Sexp.T
unlist (sexp Sexp.:> Sexp.Nil) = sexp
unlist sexp = sexp

fst :: Sexp.T -> Sexp.T
fst var_ = letMatch var_ argbody skip
  where argbody = [ Str.ArgBody  (spair Sexp.:> var "p" Sexp.:> var "q") (var "p") ]

snd :: Sexp.T -> Sexp.T
snd var_ = letMatch var_ argbody skip
  where argbody = [ Str.ArgBody  (spair Sexp.:> var "p" Sexp.:> var "q") (var "q") ]

let_ :: Str.Binder -> Sexp.T -> Sexp.T
let_ b x= Str.from $ Str.Let b x

binder :: NameSymbol.T -> Sexp.T -> Str.Binder
binder name term = Str.Binder name (var ":omega") term

-- TODO: move to record
triple :: Sexp.T -> Sexp.T -> Sexp.T -> Sexp.T
triple name value cont = Str.from $ Str.Pair name (Str.from $ Str.Pair value cont)
