module Music.Chord where

import Prelude hiding (degree)
import Data.Array
import Music.Mode
import Music.Transpose
import Music.Scale
import Music.Note
import Music.Key
import Music.Extension
import Data.Either
import Data.Foldable hiding (foldM, length, null)

data Chord = Chord PitchClass Mode (Array Extension)
instance eq_chord :: Eq Chord where
  eq c1 c2 = to_notes c1 `same_elements` to_notes c2
    where same_elements a b = null (a\\b) && null (b\\a)

instance show_chord :: Show Chord where
  show (Chord root mode exts) = (show root) <> mode_str <> foldMap show exts
    where
      mode_str = case mode of
        Major       -> "maj"
        Minor       -> "min"
        Ionian      -> "maj"
        Dorian      -> "min"
        Phrygian    -> "min"
        Lydian      -> "maj"
        Mixolydian  -> if any (\ex -> degree ex == 7) exts then "dom" else "maj"
        Aeolian     -> "min"
        Locrian     -> "min"
        Augmented   -> "aug"
        Diminished  -> "dim"
instance transpose_chord :: Transpose Chord where
  trans :: Int -> Chord -> Chord
  trans n (Chord root qual exts) = Chord (trans n root) qual exts

{-
  Converting a chord to a collection of pitches is non-trivial. THings to keep in mind:
        - The root is the lowest note
        -scale_ext :: Pitch -> Mode -> Extension -> Pitch
-}
to_notes :: Chord -> Array Pitch
to_notes (Chord root mode exts) = root_note : map (find_extension keyed_note) notes
  where
    keyed_note = Keyed {pitch: root, mode: mode} root_note
    root_note = Pitch root 4 :: Pitch
    notes = (if contains_ext 3 then [] else [Add 3])
         <> (if contains_ext 5 then [] else [Add 5])
         <> exts
    contains_ext n = any (\ex -> degree ex == n) exts

{-
  What makes a valid chord?
    - A chord cannot have a 'No x' and another modifier
    - A chord cannot have more than 7 notes
    - A chord cannot have two different extensions for the same note, i.e. Sharp 6 and a Flat 6

    TODO: 
-}
mk_chord :: PitchClass -> Mode -> Array Extension -> Either String Chord
mk_chord note mode exts = Right $ Chord note mode exts

add_ext :: Chord -> Extension -> Either String Chord
add_ext chord@(Chord note qual exts) e
  | length exts > 5 = Left "Too many notes in chord"
  | not (degree_test chord e) = Left $ "Two exts of the same degree found in chord. All extensions: "<> show (implicit_extensions e <> exts)
  | otherwise = Right $ Chord note qual (implicit_extensions e)

degree_test :: Chord -> Extension -> Boolean 
degree_test (Chord _ _ exts) e = and $ diff_degree <$> implicit_extensions e <*> exts
  where diff_degree a b = degree a /= degree b

-- Sometimes adding an extesion mean adding more than one extension:
--   * Cm9 is a chord with a C minor triad, a flat 7, and
implicit_extensions :: Extension -> Array Extension
implicit_extensions (Add 9)  = [Add 7, Add 9]
implicit_extensions (Add 11) = [Add 7, Add 9, Add 11]
implicit_extensions (Add 13) = [Add 7, Add 9, Add 11, Add 13]
implicit_extensions ext = [ext]