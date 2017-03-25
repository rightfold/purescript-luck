module Luck.Check
  ( check
  ) where

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List (List(Nil), (:))
import Data.Set (Set)
import Data.Set as Set
import Luck.Sexp (Sexp(..))
import Prelude

check :: Set String -> Sexp -> Either String Unit
check env (Atom x) = when (not $ Set.member x env) (Left "Unknown variable")
check env (List Nil) = Left "Empty list"
check env (List (f : as)) = check env f *> traverse_ (check env) as
