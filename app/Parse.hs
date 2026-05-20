module Parse
  ( Parser,
    andThen,
    wordsParser,
    manyParser,
    TupleToken (..),
    tupleParser,
    consumeWordsAndPrint,
  )
where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

lastButOne :: [a] -> Maybe a
lastButOne [] = Nothing
lastButOne [_] = Nothing
lastButOne (x : [_]) = Just x
lastButOne (_ : xs) = lastButOne xs

getSecondLastWord :: String -> String
getSecondLastWord s = fromMaybe "" $ lastButOne $ words s

type Parser s = String -> Maybe (s, String)

andThen :: Parser a -> Parser b -> Parser (a, b)
andThen p1 p2 input = do
  (a, rest) <- p1 input
  (b, rest') <- p2 rest
  return ((a, b), rest')

wordsParser :: Parser String
wordsParser x =
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

manyParser :: Parser a -> String -> [a]
manyParser p input = case p input of
  Nothing -> []
  Just (x, rest) -> x : manyParser p (dropWhile isSpace rest)

consumeWordsAndPrint :: IO ()
consumeWordsAndPrint = do
  line <- getLine
  case line of
    "" -> return ()
    _ -> do
      print (manyParser tupleParser line)
      consumeWordsAndPrint
