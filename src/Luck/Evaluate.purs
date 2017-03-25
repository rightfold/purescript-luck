module Luck.Evaluate
  ( Value(..)
  , evaluate
  ) where

import Data.Either (Either(..))
import Data.List (List(Nil), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Luck.Sexp (Sexp(..))
import Prelude

data Value
  = Sexp Sexp
  | Func (List Value -> Either String Value)

evaluate :: Map String Value -> Sexp -> Either String Value
evaluate env (Atom x) = maybe (Left "Unknown variable") Right $ Map.lookup x env
evaluate env (List Nil) = Left "Empty list"
evaluate env (List (f : as)) = do
  f' <- evaluate env f
  as' <- traverse (evaluate env) as
  case f' of
    Sexp _    -> Left "Call to non-function"
    Func func -> func as'
