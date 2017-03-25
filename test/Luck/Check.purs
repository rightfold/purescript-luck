module Test.Luck.Check
  ( main
  ) where

import Data.Either (Either(..))
import Data.List (List(Nil), (:))
import Data.Set as Set
import Luck.Check (check)
import Luck.Sexp (Sexp(..))
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

main = suite "Luck.Check" do
  suite "check" do
    test "unknown variable" do
      example (Left "Unknown variable")
              (Atom "a")
    test "unknown variable in callee" do
      example (Left "Unknown variable")
              (List $ Atom "a" : Atom "y" : Atom "z" : Nil)
    test "unknown variable in argument" do
      example (Left "Unknown variable")
              (List $ Atom "x" : Atom "a" : Atom "z" : Nil)
    test "empty list" do
      example (Left "Empty list")
              (List Nil)
    test "known variable" do
      example (Right unit)
              (Atom "x")
    test "call" do
      example (Right unit)
              (List $ Atom "x" : Atom "y" : Atom "z" : Nil)
  where
  example e s = Assert.equal e (check environment s)
  environment = Set.fromFoldable ["x", "y", "z"]
