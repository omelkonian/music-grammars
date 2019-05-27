{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Music
  ( -- euterpea
    Music(..), pattern Note', pattern Rest', Dur, Pitch, PitchClass(..), Octave
  , Music1, Note1, ToMusic1, toMusic1
  , StdLoudness(..), setDynamic
  , tempo
  , wn, hn, qn, sn, en, tn, line1, chord1, pitch, absPitch
  , play, playDev, writeToMidiFile
  , (%), (//)

  -- new
  , Rhythm, Melody, Harmony
  , Interval(..), iToEnum
  , AbstractChord, Chord, SemiChord
  , AbstractScale, Scale, SemiScale
  , (~>), (<~), (^.), (^^^) , (%>), (<|) , (#) , (~~) , (=|)
  , invertChord, invertScale, mode
  , allOctaves

  , allChords
  , maj, mi, dim, aug, majb5, mis5, sus4, sus4s5, d7sus4, maj6, m6, maj7, m7
  , d7, dim7, m7b5, mmaj7, maj9, m9, d9, d7b5, d7s5, d7b9, d7s9
  , d7b5b9, d7b5s9, d7s5b9, d7s5s9
  , sus4, d7sus4, maj7, mmaj7

  , allScales
  , major, pentatonicMajor, ionian, dorian, phrygian, lydian, mixolydian, aeolian
  , locrian, minor, harmonicMinor, melodicMinor, pentatonicMinor, blues
  , bebopDominant, bebopDorian, bebopMajor, bebopMelodicMinor, bebopHarmonicMinor
  , altered, wholeTone, halfDiminished, flamenco, persian, romanian, arabian
  , japanese, hungarian, jewish, byzantine, raga
  , doubleHarmonicMajor, lydian_s2s6, ultraphrygian, gypsyMinor, oriental
  , ionian_s2s5, locrian_bb3bb7, phrygianDominant, harmonicMajor
  )
  where

import GHC.Generics  (Generic)
import Data.List     (insert)
import qualified Data.Ratio as R

import Euterpea ( Music(..), Primitive(..), Dur, Pitch, PitchClass(..), Octave
                , Music1, ToMusic1, Note1, toMusic1
                , Control(..), PhraseAttribute(..), Dynamic(..), StdLoudness(..)
                , tempo
                , wn, hn, qn, sn, en, tn
                , line1, chord1, pitch, absPitch, transpose, trans
                , play, playDev, exportMidiFile
                , exportMidiFile, toMidi, perform
                )

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

setDynamic :: StdLoudness -> Music a -> Music a
setDynamic d = Modify (Phrase [Dyn $ StdLoudness d])

-- | Write `Music` to MIDI file.
writeToMidiFile :: ToMusic1 a => FilePath -> Dur -> Music a -> IO ()
writeToMidiFile path d = exportMidiFile path          -- write to disk
                       . toMidi . perform . toMusic1  -- convert to MIDI
                       . tempo d                      -- set tempo

pattern Note' :: Dur -> t -> Music t
pattern Note' d x = Prim (Note d x)

pattern Rest' :: Dur -> Music t
pattern Rest' d   = Prim (Rest d)

(//) :: Rational -> Rational -> Int
r1 // r2 = fromInteger $ quot (R.numerator r) (R.denominator r)
  where r = r1 / r2

(%) :: Integer -> Integer -> Rational
(%) = (R.%)

type Rhythm  = Music ()
type Melody  = Music Pitch
type Harmony = Music Chord

-- Musical structures.
data Interval = P1  | Mi2  | M2  | Mi3  | M3  | P4 | A4
              | P5  | Mi6  | M6  | Mi7  | M7  | P8
              | Mi9 | M9   | A9  | M10  | P11 | A11
              | P12 | Mi13 | M13 | Mi14 | M14 | P15
              deriving (Eq, Show, Generic, Enum, Bounded, Ord)

type Chord = [Pitch]
type Scale = [Pitch]
type SemiChord = [PitchClass]
type SemiScale = [PitchClass]
type AbstractChord = [Interval]
type AbstractScale = [Interval]

instance ToMusic1 a => ToMusic1 [a] where
  toMusic1 (m :+: m')   = toMusic1 m :+: toMusic1 m'
  toMusic1 (m :=: m')   = toMusic1 m :=: toMusic1 m'
  toMusic1 (Note' d ps) = toMusic1 $ chord1 $ Note' d <$> ps
  toMusic1 (Rest' d)    = Rest' d
  toMusic1 _            = error "toMusic1"

--------------------------------------------------------------------------------
---------------------------------- Operators -----------------------------------
--------------------------------------------------------------------------------

-- | Operator precedence.
infix  9 #, ~~
infix  7 <|
infix  6 =|
infixl 5 %>, ~>, <~

-- Constructors.
(^^^), (^.) :: Dur -> Dur
(^^^) d = 2*d / 3
(^.)  d = d + d/2

(~~) :: Dur -> Music a
(~~) = Rest'

(#) :: PitchClass -> Octave -> Pitch
pc # n = (pc, n)

(<|) :: a -> Dur -> Music a
(<|) = flip Note'

(%>) :: Music a -> Dur -> Music a
m %> d = (d~~) :+: m

-- Transformations.
class Transposable a where
  (~>), (<~) :: a -> Interval -> a

instance Transposable (Music a) where
  x ~> n = transpose (fromEnum n) x
  x <~ n = transpose (-(fromEnum n)) x

instance Transposable Pitch where
  x ~> n = trans (fromEnum n) x
  x <~ n = trans (-(fromEnum n)) x

instance Transposable PitchClass where
  pc ~> n = fst $ pc#1 ~> n
  pc <~ n = fst $ pc#6 <~ n

instance Transposable Chord where
  ps ~> n = (~> n) <$> ps
  ps <~ n = (<~ n) <$> ps

iToEnum :: Int -> Interval
iToEnum n | n < 0            = P1
          | n > fromEnum P15 = P15
          | otherwise        = toEnum n

instance Transposable Interval where
  i ~> n = iToEnum $ fromEnum i + fromEnum n
  i <~ n = iToEnum $ fromEnum i - fromEnum n

invertChord :: Chord -> Chord
invertChord (x:xs) = xs ++ [x ~> P8]
invertChord []     = []

(=|) :: PitchClass -> [Interval] -> [PitchClass]
pc =| is = (fst . (pc#0 ~>)) <$> is

--------------------------------------------------------------------------------
---------------------------------- Constants -----------------------------------
--------------------------------------------------------------------------------

-- Octaves
allOctaves :: [Octave]
allOctaves = [0..6]

allChords =
  [ maj, mi, dim, aug, majb5, mis5, sus4, sus4s5, d7sus4, maj6, m6, maj7, m7
  , d7, dim7, m7b5, mmaj7, maj9, m9, d9, d7b5, d7s5, d7b9, d7s9
  , d7b5b9, d7b5s9, d7s5b9, d7s5s9
  ] :: [AbstractChord]

-- Triads
maj = [P1, M3, P5]
mi  = [P1, Mi3, P5]
dim = [P1, Mi3, A4]
aug = [P1, M3, Mi6]
majb5 = [P1, M3, A4]
mis5 = [P1, Mi3, Mi6]
-- sus
sus4 = [P1, P4, P5]
sus4s5 = [P1, P4, Mi6]
d7sus4 = [P1, P4, P5, Mi7]
-- 6ths
maj6 = [P1, M3, P5, M6]
m6 = [P1, Mi3, P5, M6]
-- 7ths
maj7 = [P1, M3, P5, M7]
m7 = [P1, Mi3, P5, Mi7]
d7 = [P1, M3, P5, Mi7]
dim7 = [P1, Mi3, A4, M6]
m7b5 = [P1, Mi3, A4, Mi7]
mmaj7 = [P1, Mi3, P5, M7]
-- 9ths
maj9 = [P1, M3, P5, M7, M9]
m9 = [P1, Mi3, P5, Mi7, M9]
d9 = [P1, M3, P5, Mi7, M9]
-- Altered Dominants
d7b5 = [P1, M3, A4, Mi7]
d7s5 = [P1, M3, Mi6, Mi7]
d7b9 = [P1, M3, P5, Mi7, Mi9]
d7s9 = [P1, M3, P5, Mi7, A9]
d7b5b9 = [P1, M3, A4, Mi7, Mi9]
d7b5s9 = [P1, M3, A4, Mi7, A9]
d7s5b9 = [P1, M3, Mi6, Mi7, Mi9]
d7s5s9 = [P1, M3, Mi6, Mi7, A9]

allScales =
  [ major, pentatonicMajor, ionian, dorian, phrygian, lydian, mixolydian, aeolian
  , locrian, minor, harmonicMinor, melodicMinor, pentatonicMinor, blues
  , bebopDominant, bebopDorian, bebopMajor, bebopMelodicMinor, bebopHarmonicMinor
  , altered, wholeTone, halfDiminished, flamenco, persian, romanian, arabian
  , japanese, hungarian, jewish, byzantine, raga
  , doubleHarmonicMajor, lydian_s2s6, ultraphrygian, gypsyMinor, oriental
  , ionian_s2s5, locrian_bb3bb7, phrygianDominant, harmonicMajor
  ] :: [AbstractScale]

invertScale :: AbstractScale -> AbstractScale
invertScale sc' = scanl (~>) P1 (tail sc ++ [head sc])
  where sc :: AbstractScale
        sc = (\(x, y) -> y <~ x) <$> zip sc' (tail sc')

mode :: Int -> AbstractScale -> AbstractScale
mode n = last . take n . iterate invertScale

-- Major scales.
major = [P1, M2, M3, P4, P5, M6, M7]
pentatonicMajor = [P1, M2, M3, P5, M6]
ionian = mode 1 major
dorian = mode 2 major
phrygian = mode 3 major
lydian = mode 4 major
mixolydian = mode 5 major
aeolian = mode 6 major
locrian = mode 7 major

-- Minor scales.
minor = [P1, M2, Mi3, P4, P5, Mi6, Mi7]
harmonicMinor = [P1, M2, Mi3, P4, P5, Mi6, M7]
melodicMinor = [P1, M2, Mi3, P4, P5, M6, M7]
pentatonicMinor = [P1, Mi3, P4, P5, Mi7]
blues = [P1, Mi3, P4, A4, P5, Mi7]

-- Bebop scales.
bebopDominant = insert M7 mixolydian
bebopDorian = mode 5 bebopDominant
bebopMajor = insert Mi6 major
bebopMelodicMinor = insert Mi6 melodicMinor
bebopHarmonicMinor = mode 6 bebopMelodicMinor

-- Exotic scales.
persian = [P1, Mi2, M3, P4, P5, Mi6, M7]
flamenco = persian
romanian = [P1, M2, Mi3, A4, P5, M6, Mi7]
arabian = [P1, M2, Mi3, P4, A4, Mi6, M7]
japanese = [P1, M2, P4, A4, Mi6, M6, M7]
hungarian = [P1, M2, Mi3, A4, P5, Mi6, M7]
jewish = [P1, Mi2, M3, P4, P5, Mi6, Mi7]
byzantine = [P1, Mi2, M3, P4, P5, Mi6, M7]
raga = [P1, Mi2, Mi3, P4, P5, Mi6, Mi7]

doubleHarmonicMajor = [P1, Mi2, M3, P4, P5, Mi6, M7]
lydian_s2s6         = mode 2 doubleHarmonicMajor
ultraphrygian       = mode 3 doubleHarmonicMajor
gypsyMinor          = mode 4 doubleHarmonicMajor
oriental            = mode 5 doubleHarmonicMajor
ionian_s2s5         = mode 6 doubleHarmonicMajor
locrian_bb3bb7      = mode 7 doubleHarmonicMajor

-- Other scales.
altered = [P1, Mi2, Mi3, M3, A4, Mi6, Mi7]
wholeTone = [P1, M2, M3, A4, Mi6, Mi7]
halfDiminished = mode 6 melodicMinor
phrygianDominant = mode 5 harmonicMinor
harmonicMajor = [P1, M2, M3, P4, P5, Mi6, M7]
