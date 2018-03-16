module NeckData (
  NeckData,
  fret_x,
  fret_marker,
  str_y
) where

import Prelude
import Fret
import Math (pow)
import Data.Int (toNumber)
import Data.Maybe

type NeckData =
  { width     :: Number
  , height    :: Number
  , num_frets :: Int }

fret_x :: NeckData -> Number -> Number
fret_x neck_data x = w * (1.0 - pow t x) / (1.0 - pow t n)
  where
    t = pow 2.0 (-1.0/12.0)
    w = neck_data.width
    n = toNumber neck_data.num_frets

fret_marker :: NeckData -> Fret -> Number
fret_marker neck_data = case _ of
  Open   -> 0.0
  Fret x -> (f (toNumber x) + f (toNumber x - 1.0)) * 0.5
    where f = fret_x neck_data

str_y :: NeckData -> Number -> Number
str_y neck_data y = (y * 2.0 + 1.0) * h / 12.0
  where h = neck_data.height
