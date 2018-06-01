module Fingering where

import Data.Argonaut.Decode
import Data.Lattice
import Data.Maybe
import Data.Ring
import Data.Traversable
import Data.Tuple
import Debug.Trace
import Fret
import Interval
import Music.Transpose
import NeckData
import Point
import Prelude hiding (join,bottom)

import DOM.Event.Types (MouseEvent)
import Data.Array (catMaybes, foldM, index, mapMaybe, zipWith, (..))
import Data.Foldable (intercalate, null, length)
import Data.Int (toNumber)
import Data.String ()
import Math (pow, sqrt)

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
instance show_fingering :: Show (ChordF (Maybe Fret)) where
  show chord = intercalate "-" $ map (maybe "x" show) chord

shift_fingering :: Int -> Fingering -> Fingering
shift_fingering n fingering =
  if meet range (interval (Fret 0) (Fret 11)) == EmptyInterval
    then (map<<<map) (_ - (Fret 12)) new_fingering
    else new_fingering
  where
    a = trace (show range) id
    range = fret_interval new_fingering
    new_fingering = (map<<<map) (_ + (Fret n)) fingering :: Fingering

to_array :: ChordF ~> Array
to_array (Fingering c) = [c.e4, c.b3, c.g3, c.d3, c.a2, c.e2]

is_open :: Fingering -> Boolean
is_open = any (eq 0.0 <<< fst) <<< to_points

fret_interval :: Fingering -> Interval Fret
fret_interval fingering = foldl join bottom frets
  where frets = map singleton $ catMaybes (to_array fingering)

type FingeringData =
  { fingering :: Fingering
  , centeroid :: Point }

cache_centeroid :: NeckData -> Fingering -> Maybe FingeringData
cache_centeroid neck chord = do
  centeroid <- centeroid_chord neck chord
  pure $ { fingering: chord, centeroid: centeroid}

to_points :: Fingering -> Array Point
to_points fingering = catMaybes $ zipWith maybe_point xs (0..5)
  where
    xs = (map<<<map) (\(Fret n) -> n) $ to_array fingering

    maybe_point :: Maybe Int -> Int -> Maybe Point
    maybe_point mx y = map (flip point_int y) mx

centeroid_chord :: NeckData -> Fingering -> Maybe Point
centeroid_chord neck_data chord =
  if null points then Nothing else Just $ factor (1.0 / toNumber (length points)) (sum points)
  where points = map (fret_transformation neck_data) $ to_points chord

closest_index :: NeckData -> Point -> Array FingeringData -> Maybe Int
closest_index neck point fings = foldM f 0 (0..(length fings - 1))
  where
    f :: Int -> Int -> Maybe Int
    f i j = do
      di <- dist <$> index fings i
      dj <- dist <$> index fings j
      pure $ if di < dj then i else j

    dist :: FingeringData -> Number
    dist fd = distance point fd.centeroid

get_closest :: forall f. Foldable f => NeckData -> Point -> f FingeringData -> Maybe FingeringData
get_closest neck_data point chords =
  minimumBy (comparing (\cd -> distance point cd.centeroid)) chords

