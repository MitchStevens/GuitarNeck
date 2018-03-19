
module Music.Note (
  PitchClass (..),
  Pitch (..),
  pcToInt,
  pitch,
  mk_pitch,
  semitones
) where

import Prelude
import Data.Semigroup
import Data.Array
import Data.Enum
import Data.Maybe
import Partial.Unsafe (unsafePartial)
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Data.Generic.Rep.Enum
import Data.String
import Music.Transpose

data PitchClass = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B
derive instance generic_pitchclass  :: Generic PitchClass _
derive instance ord_pitchclass      :: Ord PitchClass
instance eq_pitchclass              :: Eq PitchClass where
  eq x y = (pcToInt x) == (pcToInt y)
instance semigroup_pitchclass       :: Semigroup PitchClass where
  append p1 p2 = pitch (pcToInt p1 + pcToInt p2)
instance show_pitchclass            :: Show PitchClass where
  show pc = case pcToInt pc of
    0 ->  "C"
    1 ->  "C#"
    2 ->  "D"
    3 ->  "E♭"
    4 ->  "E"
    5 ->  "F"
    6 ->  "F#"
    7 ->  "G"
    8 ->  "A♭"
    9 ->  "A"
    10 -> "B♭"
    11 -> "B"
    _  -> "A terrible error has occured. This should be impossible."
instance enum_pitchclass            :: Enum PitchClass where
  succ = genericSucc
  pred = genericPred
instance bounded_pitchclass         :: Bounded PitchClass where
  bottom = C
  top    = B
instance transpose_pitchclass       :: Transpose PitchClass where
  trans n p = pitch (pcToInt p + n)

pcToInt :: PitchClass -> Int
pcToInt c = case c of
  C  -> 0
  Cs -> 1
  Db -> 1
  D  -> 2
  Ds -> 3
  Eb -> 3
  E  -> 4
  F  -> 5
  Fs -> 6
  Gb -> 6
  G  -> 7
  Gs -> 8
  Ab -> 8
  A  -> 9
  As -> 10
  Bb -> 10
  B  -> 11

pitch :: Int -> PitchClass
pitch n = unsafePartial (fromJust $ index notes (mod12 n))
  where
    notes = [C, Db, D, Eb, E, F, Fs, G, Gs, A, Bb, B]
    mod12 x = mod (mod x 12 + 12) 12

data Pitch = Pitch PitchClass Int
instance eq_pitch :: Eq Pitch where
  eq n1 n2 = semitones n1 == semitones n2
instance ord_pitch :: Ord Pitch where
  compare = comparing semitones
instance transpose_pitch :: Transpose Pitch where
  trans n x = mk_pitch (pitch tones) (div tones 12)
    where tones = semitones x + n
instance show_pitch :: Show Pitch where
  show (Pitch pc oct) = show pc <> show oct

mk_pitch :: PitchClass -> Int -> Pitch
mk_pitch root octave = Pitch root octave

semitones :: Pitch -> Int
semitones (Pitch pc octave) = (pcToInt pc) + (octave * 12)