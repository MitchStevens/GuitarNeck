module Chord where

import Prelude
import Data.Semigroup
import Data.Monoid
import Data.Tuple
import Data.Int (toNumber)

newtype Point = Point { x :: Number, y :: Number }
instance semigroup_point :: Semigroup Point where
  append (Point p1) (Point p2) = Point {x: p1.x+p2.x, y: p1.y+p2.y}
instance monoid_point :: Monoid Point where
  mempty = Point {x: 0.0, y:0.0}

type FretNum = Int
data StringNum = E4 | B3 | G3 | D3 | A2 | E2
type Fingering = Tuple StringNum FretNum
type Chord = Array Fingering

str_to_num :: StringNum -> Number
str_to_num = case _ of
  E4 -> 0.0
  B3 -> 1.0
  G3 -> 2.0
  D3 -> 3.0
  A2 -> 4.0
  E2 -> 5.0