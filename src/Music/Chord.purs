module Music.Chord where

import Prelude hiding (degree)
import Data.Array
import Data.Either
import Data.Foldable hiding (foldM, length, null)
import Data.Maybe

import Music.Mode
import Music.Transpose
import Music.Scale
import Music.Note
import Music.Key
import Music.Extension

data Chord = Chord PitchClass Mode (Array Extension)
instance eq_chord :: Eq Chord where
  eq c1 c2 = true
instance show_chord :: Show Chord where
  show chord = m.root <> chord_suffix chord
    where m = markup chord

chord_suffix :: Chord -> String
chord_suffix chord = m.mode <> m.head_ext <> fold m.tail_ext
  where m = markup chord

instance transpose_chord :: Transpose Chord where
  trans :: Int -> Chord -> Chord
  trans n (Chord root mode exts) = Chord (trans n root) mode exts

type ChordMarkup =
  { root :: String
  , mode :: String
  , head_ext :: String
  , tail_ext :: Array String
  }

markup :: Chord -> ChordMarkup
markup (Chord root Major [])        = mk_markup root "" []
markup (Chord root Major exts)      = mk_markup root "maj" exts
markup (Chord root Minor exts)      = mk_markup root "m" exts
markup (Chord root Mixolydian exts) = mk_markup root "" exts
markup (Chord root Diminished exts) = mk_markup root "dim" exts
markup (Chord root Augmented exts)  = mk_markup root "aug" exts
markup (Chord root _ exts)          = mk_markup root "???" exts

mk_markup :: PitchClass -> String -> Array Extension -> ChordMarkup
mk_markup root mode exts = case uncons (map show exts) of
  Just {head: x, tail: xs} -> mk_exts x (map bracket xs)
  Nothing                  -> mk_exts "" []
  where
    mk_exts x y = {root:show root, mode: mode, head_ext:x, tail_ext:y}
    bracket x = "("<>x<>")"