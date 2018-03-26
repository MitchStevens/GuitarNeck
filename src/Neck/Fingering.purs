module Fingering where

import Fret
import NeckData
import Prelude

import Data.Argonaut.Decode
import Data.Array (catMaybes, zipWith)
import Data.Foldable (intercalate, null, length)
import Data.Int (toNumber)
import Data.Maybe
import Data.Ring
import Data.String ()
import Data.Traversable
import Data.Tuple

import DOM.Event.Types (MouseEvent)
import Math (pow, sqrt)

newtype Point = Point { x :: Number, y :: Number }
instance semiring_point :: Semiring Point where
  zero = Point {x: 0.0, y: 0.0}
  one  = Point {x: 1.0, y: 1.0}
  add (Point p1) (Point p2) = Point {x: p1.x+p2.x, y: p1.y+p2.y}
  mul (Point p1) (Point p2) = Point {x: p1.x*p2.x, y: p1.y*p2.y}
instance ring_point :: Ring Point where
  sub (Point p1) (Point p2) = Point {x: p1.x-p2.x, y: p1.y-p2.y}
instance show_point :: Show Point where
  show (Point p) = "("<> show p.x <>", "<> show p.y <>")"

factor :: Number -> Point -> Point
factor n (Point p) = Point {x: n*p.x, y: n*p.y}

distance :: Point -> Point -> Number
distance p1 p2 = sqrt $ pow p.x 2.0 + pow p.y 2.0
  where Point p = p1 - p2

point :: Number -> Number -> Point
point x y = Point {x: x, y: y}

----------------------------------

newtype ChordF a = Fingering {e4 :: a,  b3 :: a, g3 :: a, d3 :: a, a2 :: a, e2 :: a}
instance functor_chord :: Functor ChordF where
  map f (Fingering c) = Fingering {e4: f c.e4, b3: f c.b3, g3: f c.g3, d3: f c.d3, a2: f c.a2, e2: f c.e2}
instance apply_chord :: Apply ChordF where
  apply (Fingering f) (Fingering x) = Fingering
    { e4: f.e4 x.e4
    , b3: f.b3 x.b3
    , g3: f.g3 x.g3
    , d3: f.d3 x.d3
    , a2: f.a2 x.a2
    , e2: f.e2 x.e2 }
instance applicative_chord :: Applicative ChordF where
  pure c = Fingering {e4: c, b3: c, g3: c, d3: c, a2: c, e2: c}
instance foldable_chord :: Foldable ChordF where
  foldl f z chord = foldl f z $ to_array chord
  foldr f z chord = foldr f z $ to_array chord
  foldMap f chord = foldMap f $ to_array chord
type Fingering = ChordF (Maybe Fret)
instance show_chord :: Show (ChordF (Maybe Fret)) where
  show chord = intercalate "-" $ map (maybe "x" show) chord
instance transpose_chord :: Transpose (ChordF (Maybe Fret)) where
  trans n = 

to_array :: ChordF ~> Array
to_array (Fingering c) = [c.e4, c.b3, c.g3, c.d3, c.a2, c.e2]

{-

Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t 

newtype ChordF a = ChordF {e4 :: a,  b3 :: a, g3 :: a, d3 :: a, a2 :: a, e2 :: a} deriving Functor
type Chord = Chord Int
transpose :: Int -> Chord Int -> Chord Int
transpose n = fmap (+n)

-}

type FingeringData =
  { fingering :: Fingering
  , centeroid :: Point
  }

cache_centeroid :: NeckData -> Fingering -> Maybe FingeringData
cache_centeroid neck chord = do
  centeroid <- centeroid_chord neck chord
  pure $ { fingering: chord, centeroid: centeroid}

to_points :: NeckData -> Fingering -> Array Point
to_points neck chord = catMaybes $ zipWith maybe_point xs ys
  where
    xs = (map <<< map) (fret_marker neck) $ to_array chord
    ys = map (str_y neck) [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]

    maybe_point :: Maybe Number -> Number -> Maybe Point
    maybe_point mx y = do
      x <- mx
      pure $ point x y

centeroid_chord :: NeckData -> Fingering -> Maybe Point
centeroid_chord neck_data chord =
  if null points then Nothing else Just $ factor (1.0 / toNumber (length points)) (sum points)
  where points = to_points neck_data chord

get_closest :: forall f. Foldable f => NeckData -> Point -> f FingeringData -> Maybe FingeringData
get_closest neck_data point chords =
  minimumBy (comparing (\cd -> distance point cd.centeroid)) chords