module NeckData (
  NeckData,
  neck_transformation,
  fret_transformation
) where

import Data.Maybe
import Fret
import Point
import Prelude

import Data.Int (round, toNumber)
import Math (pow)

type NeckData =
  { x_offset  :: Number
  , y_offset  :: Number
  , width     :: Number
  , height    :: Number
  , num_frets :: Int }

fret_x :: NeckData -> Number -> Number
fret_x neck x = neck.x_offset + w * (1.0 - pow t x) / (1.0 - pow t n)
  where
    t = pow 2.0 (-1.0/12.0)
    w = neck.width
    n = toNumber neck.num_frets

fret_marker :: NeckData -> Fret -> Number
fret_marker neck fret = nat_x
    where 
      f = fret_x neck
      nat_x = case fret of
        Fret 0 -> 0.0
        Fret x -> (f (toNumber x) + f (toNumber x - 1.0)) * 0.5

str_y :: NeckData -> Number -> Number
str_y neck y = neck.y_offset + (y * 2.0 + 1.0) * h / 12.0
  where h = neck.height

neck_transformation :: NeckData -> Transformation
neck_transformation neck (Point p) = point (fret_x neck p.x) (str_y neck  p.y)

fret_transformation :: NeckData -> Transformation
fret_transformation neck (Point p) =
  point (fret_marker neck (Fret $ round p.x)) (str_y neck p.y)

