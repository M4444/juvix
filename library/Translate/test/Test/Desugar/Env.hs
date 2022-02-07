module Test.Desugar.Env (top) where

import Data.List.NonEmpty (fromList)
import qualified Juvix.Desugar.Env as Env
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Transition as Structure
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "testing berlin pipeline passes functions"
    [sexpsByModuleTest]

defaultName :: NameSymbol.T = "default"

sexpsByModuleTest :: T.TestTree
sexpsByModuleTest =
  T.testGroup
    "Testing sexpsByModule utility"
    [ T.testCase "passing empty sexps evals to empty" $ do
        (defaultName, []) :| [] T.@=? Env.sexpsByModule defaultName [],
      T.testCase "passing only in-package sexps are ignored" $ do
        let inPackage = Structure.InPackage "foo" |> Sexp.serialize
        (defaultName, []) :| [] T.@=? Env.sexpsByModule defaultName (take 10 (repeat inPackage)),
      T.testCase "sexps following an in-package sexp are collected into a module" $ do
        let sexp = Structure.LambdaCase [] |> Sexp.serialize
        let following = take 10 (repeat sexp)
        let package = Structure.InPackage "foo" |> Sexp.serialize
        fromList [("foo", following)] T.@=? (Env.sexpsByModule defaultName (package : following)),
      T.testCase "in-package prefixes followed by in-package forms are ignored" $ do
        let sexp = Structure.LambdaCase [] |> Sexp.serialize
        let following = take 10 (repeat sexp)
        let package1 = Structure.InPackage "foo1" |> Sexp.serialize
        let package2 = Structure.InPackage "foo2" |> Sexp.serialize
        fromList [("foo2", following)] T.@=? (Env.sexpsByModule defaultName (package1 : package2 : following)),
      T.testCase "consecutive packages are captured" $ do
        let sexp = Structure.LambdaCase [] |> Sexp.serialize
        let following = take 10 (repeat sexp)
        let package1 = Structure.InPackage "foo1" |> Sexp.serialize
        let package2 = Structure.InPackage "foo2" |> Sexp.serialize
        let forms1 = package1 : following
        let forms2 = package2 : following
        fromList [("foo1", following), ("foo2", following)] T.@=? (Env.sexpsByModule defaultName (forms1 <> forms2))
    ]
