module Point (
  Point,
  Transformation,
  _x,
  _y,
  factor,
  distance,
  point_int
)where

import Math
import Prelude

import Control.Biapply
import Control.Biapplicative

import Data.Bifunctor
import Data.Int (toNumber)
import Data.Lens
import Data.Tuple

type Point = Tuple Number Number
type Transformation = Point -> Point

_x = _1 :: Lens' Point Number
_y = _2 :: Lens' Point Number

factor :: Number -> Point -> Point
factor n = bimap (n*_) (n*_)

distance :: Point -> Point -> Number
distance p1 p2 = sqrt (x*x + y*y)
  where Tuple x y = p1 - p2

point_int :: Int -> Int -> Point
point_int x y = bimap toNumber toNumber $ Tuple x y