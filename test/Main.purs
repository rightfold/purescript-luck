module Test.Main
  ( main
  ) where

import Prelude
import Test.Luck.Check as Luck.Check
import Test.Luck.Sexp as Luck.Sexp
import Test.Unit.Main (runTest)

main = runTest do
  Luck.Check.main
  Luck.Sexp.main
