module Music.Scale (
  as_array,
  in_scale,
  pitch_as_degree,
  find_extension,
  find_degree,
  belongs_to,
  Scale (..)
) where

import Data.Array
import Data.Enum
import Data.Foldable
import Data.Maybe
import Data.Maybe
import Data.Monoid
import Debug.Trace
import Music.Degree
import Music.Extension
import Music.Key
import Music.Mode
import Music.Note
import Music.Transpose
import Partial.Unsafe
import Prelude hiding (degree)

import Data.Newtype (overF)
import Data.String (splitAt)


{-
A scale is a set of notes, ordered by pitch
Operations of scales
  Querying if a note is in a scale
  get the note in the scale
  taking a subscale
  range of a scale
-}
data Scale = Scale Pitch Mode
derive instance eq_scale :: Eq Scale
instance show_scale :: Show Scale where
  show (Scale pitch mode) = show pitch <> show mode <> " scale"

--scale_key :: Scale -> Key
--scale_key (Scale root mode) = {pitch: root.root, mode: mode}

{- Return the scale as a list of notes -}
as_array :: Scale -> Int -> Array Pitch
as_array (Scale root mode) n = [root] <> scanl (flip trans) root (take (n-1) ints)
  where ints = power (intervals mode) (1 + div n 7)

in_scale :: Scale -> Pitch -> Maybe Int
in_scale scale (Pitch pc1 _) = findIndex (\(Pitch pc2 _) -> pc1 == pc2) (as_array scale 7)

find_extension :: Keyed Pitch -> Extension -> Pitch
find_extension (Keyed key note) ext = unsafePartial $ trans accidental $ get_note scale deg
  where
    scale = Scale note key.mode
    deg = (degree ext - 1)
    accidental = case ext of
      Flat _  -> -1
      Add _   -> 0
      Sharp _ -> 1
      Sus2    -> 0
      Sus4    -> 0

find_degree :: Keyed Pitch -> Degree -> Pitch
find_degree (Keyed key pitch) degree = unsafePartial $ get_note scale (fromEnum degree)
  where scale = Scale pitch key.mode

get_note :: Partial => Scale -> Int -> Pitch
get_note (Scale pitch mode) n =
  if n < 0 then unsafeCrashWith "Index must be > 0"
    else trans (12 * octaves + ints) pitch
      where
        m = num_notes mode
        octaves = div n m
        ints = sum $ take (mod n m) $ intervals mode

{- Given a root note and a Keyed Extension, return whatever note is the root + the extension
scale_ext :: Scale -> Extension -> Pitch
scale_ext scale ext = trans accedental $ infinite_scale scale !! (degree ext - 1)
  where
    accedental = case ext of
      Sharp _ -> 1
      Add   _ -> 0
      Flat  _ -> -1
-}
belongs_to :: PitchClass -> Key -> Maybe Degree
belongs_to pc1 key = do
  index <- findIndex (\(Pitch pc2 _) -> pcToInt pc1 == pcToInt pc2) notes
  toEnum index
    where notes = as_array (Scale (mk_pitch key.pitch 0) key.mode) 7

--degree_as_pitch :: Keyed ScaleDegree -> PitchClass
--degree_as_pitch (Keyed key degree) = index (fromEnum degree) to_notes

{- Given some keyed pitch, return Just a scale degree if the note is in the key, If is is not in the key, Return Nothing
  Ex.
    Keyed (C, Major)  C  -> Just I
    Keyed (Bb, Minor) Db -> Just III
    Keyed (A, Major)  Eb -> Nothing -}
pitch_as_degree :: Keyed Pitch -> Maybe Degree
pitch_as_degree (Keyed key (Pitch pc _)) = belongs_to pc key