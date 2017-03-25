module Luck.Sexp
  ( Sexp(..)

  , fromString
  ) where

import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.String as String
import Data.Tuple (Tuple(..))
import Prelude

--------------------------------------------------------------------------------

data Sexp
  = Atom String
  | List (List Sexp)

derive instance eqSexp :: Eq Sexp
derive instance ordSexp :: Ord Sexp
derive instance genericSexp :: Generic Sexp
instance showSexp :: Show Sexp where show = gShow

--------------------------------------------------------------------------------

fromString :: String -> Either String Sexp
fromString = unString >>> fromString' >=> case _ of
  Tuple value Nil -> Right value
  _ -> Left "Expected end of file"

fromString' :: List Char -> Either String (Tuple Sexp (List Char))
fromString' Nil = Left "Unexpected end of file"
fromString' (c : cs) | isSpace c = fromString' cs
fromString' (c : cs) | isAlpha c =
  case List.span isAlpha cs of
    {init, rest} -> Right $ Tuple (Atom <<< string $ (c : init)) rest
fromString' ('(' : cs) = go Nil cs
  where
  go :: List Sexp -> List Char -> Either String (Tuple Sexp (List Char))
  go acc (')' : cs') = Right <<< Tuple `flip` cs' <<< List $ List.reverse acc
  go acc cs' = fromString' cs' >>= case _ of
    Tuple value cs'' -> go (value : acc) cs''
fromString' _ = Left "Unexpected token"

isSpace :: Char -> Boolean
isSpace c = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t'

isAlpha :: Char -> Boolean
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

--------------------------------------------------------------------------------

unString :: String -> List Char
unString = String.toCharArray >>> List.fromFoldable

string :: List Char -> String
string = String.fromCharArray <<< Array.fromFoldable
