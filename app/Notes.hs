module Notes where

-- Notes types
data PitchClass = C | D | E | F | G | A | B
  deriving (Show, Eq, Ord, Enum, Bounded)

allPitches :: [PitchClass]
allPitches = [C .. B]

data Modification = Flat | Natural | Sharp
  deriving (Show, Eq)

data Note = Note PitchClass Modification
  deriving (Show, Eq)

primitiveDegrees :: [Int]
primitiveDegrees = [0, 2, 4, 5, 7, 9, 11]

noteDegree :: Note -> Int
noteDegree (Note pitch modification)
  | modification == Flat = primDeg - 1
  | modification == Natural = primDeg
  | modification == Sharp = primDeg + 1
  where
    primDeg = primitiveDegrees !! (fromEnum pitch)

-- KeySignature represents the circle of fifths in the sharp direction
data MajorKeySignature = CM | GM | DM | AM | EM | BM | FsGbM | DbM | AbM | EbM | BbM | FM
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Give the note
keyTransform :: MajorKeySignature -> PitchClass -> Note
keyTransform key note = undefined

-- Need to (Note -> Note) (correct )
movePitchClass :: Int -> PitchClass -> PitchClass
movePitchClass steps pc =
  toEnum $ (fromEnum pc + steps) `mod` 7

applyKeySignature :: Int -> [PitchClass] -> [PitchClass]
applyKeySignature index pc = undefined

isSharpened :: MajorKeySignature -> [Bool]
isSharpened mks =
  map
    ( \(i, p) ->
        False
    )
    (zip [C .. B] [C .. B]) -- [C .. B :: PitchClass]
  where
    keyIndex = fromEnum mks
