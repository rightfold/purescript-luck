module Test.Luck.Sexp
  ( main
  ) where

import Data.Either (Either(..))
import Data.List (List(Nil), (:))
import Luck.Sexp (Sexp(..), fromString)
import Prelude
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

main = suite "Luck.Sexp" do
  suite "fromString" do
    test "empty" do
      example (Left "Unexpected end of file")
              ""
    test "unclosed list" do
      example (Left "Unexpected end of file")
              "("
    test "junk" do
      example (Left "Unexpected token")
              ")"
    test "atom" do
      example (Right (Atom "atom"))
              "atom"
    test "empty list" do
      example (Right (List Nil))
              "()"
    test "flat list" do
      example (Right (List $ Atom "a" : Atom "b" : Atom "c" : Nil))
              "(a b c)"
    test "nested lists" do
      example (Right (List $ List Nil : Atom "a" : List (List Nil : Nil) : Nil))
              "(() a (()))"
  where example e s = Assert.equal e (fromString s)
