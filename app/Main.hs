module Main (main) where

import Data.Char (isSpace)
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

munchString :: Parser String
munchString x =
  Just
    ( takeWhile (not . isSpace) x,
      dropWhile isSpace (dropWhile (not . isSpace) x)
    )

data TupleToken = TupleToken Int Int
  deriving (Show)

tupleParser :: Parser TupleToken
tupleParser "" = Nothing
tupleParser ('(' : rest) =
  case break (== ',') rest of
    (a, ',' : restmiddle) ->
      case break (== ')') restmiddle of
        (b, ')' : remainder) -> Just (TupleToken (read a) (read b), remainder)
        _noFinalBracket -> Nothing
    _finally -> Nothing
tupleParser _ = Nothing

main :: IO ()
main = do
  input <- getLine
  case input of
    "" -> return ()
    _ -> do
      print (tupleParser input)
      main
