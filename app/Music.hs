module Music
  ( sampleRate,
    maxAmplitude,
    notePitch,
    generatePCM,
    playTones,
  )
where

import Data.Binary.Put (putInt16le, runPut)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import GHC.Real (reduce)
import System.IO (hClose)
import System.Process (runInteractiveCommand, waitForProcess)

sampleRate :: Double
sampleRate = 44100

maxAmplitude :: Double
maxAmplitude = 32767

sin44khz :: Double -> Integer -> Int16
sin44khz hz time =
  round
    ( maxAmplitude
        * sin (2 * pi * hz * fromIntegral time / sampleRate)
    )

-- A note in Hz of `noteDegree` of a
-- arbitrary-degree well-tempered pitch space
-- based at `referenceHz`
notePitch :: Integer -> Integer -> Double -> Double
notePitch noteDegree base referenceHz =
  2.0 ** (fromIntegral noteDegree / fromIntegral base)
    * referenceHz

listSin44khz :: [Double] -> Integer -> [Int16]
listSin44khz list t = map (\n -> sin44khz n t) list

generatePCM :: Integer -> Double -> BL.ByteString
generatePCM noteDegree duration =
  runPut $
    mapM_ (putInt16le . sample) [0 .. round (duration * sampleRate) :: Integer]
  where
    sample t = foldr (+) 0 (listSin44khz [notePitch noteDegree 12 440] t)

playTones :: [(Integer, Double)] -> IO ()
playTones tones = do
  let pcm = BL.concat (map (uncurry generatePCM) tones)
  (aplayStdin, _, aplayStderr, aplayProcess) <- runInteractiveCommand "aplay -r 44100 -f S16_LE -c 1"
  BL.hPut aplayStdin pcm
  hClose aplayStdin
  errors <- BL.hGetContents aplayStderr
  waitForProcess aplayProcess
  BL.putStr errors
