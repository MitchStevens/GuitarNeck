module Music.Mode (
  Mode (..),
  num_notes,
  intervals
) where

import Prelude
import Music.Degree
import Data.Array
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Data.Generic.Rep.Enum
import Data.Enum

data Mode
  = Major
  | Minor
  | Ionian	 
  | Dorian	 
  | Phrygian	 
  | Lydian	 
  | Mixolydian	 
  | Aeolian	 
  | Locrian	 
  | Augmented
  | Diminished
derive instance generic_mode  :: Generic Mode _
derive instance eq_mode       :: Eq Mode
derive instance ord_mode      :: Ord Mode
instance show_mode            :: Show Mode where
  show = genericShow
instance enum_pitch           :: Enum Mode where
  succ = genericSucc
  pred = genericPred
instance bounded_pitch        :: Bounded Mode where
  bottom = Major
  top    = Diminished
instance boundedenum_mode     :: BoundedEnum Mode where
    cardinality = Cardinality 11
    toEnum      = defaultToEnum
    fromEnum    = defaultFromEnum

intervals :: Mode -> Array Int
intervals = case _ of
  Major      -> scale_mode I
  Minor      -> scale_mode VI
  Ionian     -> scale_mode I
  Dorian     -> scale_mode II
  Phrygian   -> scale_mode III
  Lydian     -> scale_mode IV
  Mixolydian -> scale_mode V
  Aeolian    -> scale_mode VI
  Locrian    -> scale_mode VII
  Diminished -> [2, 1, 2, 1, 2, 1, 2, 1]
  Augmented  -> [3, 1, 3, 1, 3, 1]

{- Get a mode of the major scale, 1 is major, 2 is Dorian, 3 is Phyrigian, etc. starting from a root pitch -}
scale_mode :: Degree -> Array Int
scale_mode degree =
  let major_intervals = [2, 2, 1, 2, 2, 2, 1]
      rotate n list = (drop n list) <> (take n list)
  in rotate (fromEnum degree) major_intervals

num_notes :: Mode -> Int
num_notes mode = case mode of
  Diminished -> 8
  Augmented  -> 6
  _          -> 7