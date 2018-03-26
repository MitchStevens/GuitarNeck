module Fret where

import Prelude
import Music.Transpose
import Data.Maybe (Maybe(..), fromMaybe)

newtype Fret = Fret Int
instance show_fret :: Show Fret where
  show (Fret n) = show n
instance transpose_fret :: Transpose Fret where
  trans n (Fret x) = Fret $ mod12 (n + x)