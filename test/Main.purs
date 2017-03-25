module Test.Main
  ( main
  ) where

import Test.Luck.Sexp as Luck.Sexp
import Test.Unit.Main (runTest)

main = runTest do
  Luck.Sexp.main
