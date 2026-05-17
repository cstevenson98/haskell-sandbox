module RealWorld.Chap1.Interact (wordCount) where

wordCount :: String -> String
wordCount input = show (length (lines input)) ++ "\n"
