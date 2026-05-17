module Main (main) where

import Data.Maybe (fromMaybe)

-- Takes the second last element or Nothing
lastButOne :: [a] -> Maybe a
lastButOne [] = Nothing
lastButOne [_] = Nothing
lastButOne (x : [_]) = Just x
lastButOne (_ : xs) = lastButOne xs

getSecondLastWord :: String -> String
getSecondLastWord s = fromMaybe "" $ lastButOne $ words s

type Parser s = String -> Maybe (s, String)

basicTupleParser :: Parser (Int, Int)
basicTupleParser [] = Nothing
basicTupleParser (x : []) = Nothing
basicTupleParser (x : [xs]) = Nothing
basicTupleParser (x : xs)
  | x == '(' = Nothing

main :: IO ()
main = interact getSecondLastWord
