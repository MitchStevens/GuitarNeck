module Point (
  Point (..),
  Transformation,
  factor,
  distance,
  point,
  point_int
)where

import Prelude
import Math
import Data.Int (toNumber)

newtype Point = Point { x :: Number, y :: Number }
derive instance eq_point  :: Eq Point
derive instance ord_point :: Ord Point
instance semiring_point :: Semiring Point where
  zero = Point {x: 0.0, y: 0.0}
  one  = Point {x: 1.0, y: 1.0}
  add (Point p1) (Point p2) = Point {x: p1.x+p2.x, y: p1.y+p2.y}
  mul (Point p1) (Point p2) = Point {x: p1.x*p2.x, y: p1.y*p2.y}
instance ring_point :: Ring Point where
  sub (Point p1) (Point p2) = Point {x: p1.x-p2.x, y: p1.y-p2.y}
instance show_point :: Show Point where
  show (Point p) = "("<> show p.x <>", "<> show p.y <>")"

type Transformation = Point -> Point

factor :: Number -> Point -> Point
factor n (Point p) = Point {x: n*p.x, y: n*p.y}

distance :: Point -> Point -> Number
distance p1 p2 = sqrt $ pow p.x 2.0 + pow p.y 2.0
  where Point p = p1 - p2

point :: Number -> Number -> Point
point x y = Point {x: x, y: y}

point_int :: Int -> Int -> Point
point_int x y = point (toNumber x) (toNumber y)