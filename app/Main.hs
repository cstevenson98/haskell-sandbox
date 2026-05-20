module Main (main) where

import Music (playTones)
import Notes

main :: IO ()
main =
  playTones
    [ (0, 0.5),
      (2, 0.5),
      (4, 0.5),
      (5, 0.5),
      (7, 0.5),
      (9, 0.5),
      (11, 0.5)
    ]
