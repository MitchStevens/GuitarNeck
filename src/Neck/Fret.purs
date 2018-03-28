module Fret where

import Prelude
import Music.Transpose
import Data.Maybe (Maybe(..), fromMaybe)

newtype Fret = Fret Int
instance eq_fret :: Eq Fret where
  eq (Fret x) (Fret y) = x == y
instance ord_fret :: Ord Fret where
  compare = comparing (\(Fret x) -> x)
instance show_fret :: Show Fret where
  show (Fret n) = show n
instance transpose_fret :: Transpose Fret where
  trans n (Fret x) = Fret $ mod12 (n + x)
instance semiring_fret :: Semiring Fret where
  zero = Fret zero
  one  = Fret one
  add (Fret x) (Fret y) = Fret (x + y)
  mul (Fret x) (Fret y) = Fret (x * y)
instance ring_fret :: Ring Fret where
  sub (Fret x) (Fret y) = Fret (x - y)
