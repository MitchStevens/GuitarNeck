module UI.ChordDiagramCanvas (
  paint_chord_diagram--,
  --wipe_chord_diagram
) where

import Control.Monad.Eff
import Data.Tuple
import Fingering
import Graphics.Canvas
import Point
import Point
import Prelude

import Data.Array ((..))
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Data.Int (toNumber)

font_size = 16 :: Int
v_margin = 0.15 :: Number
num_strings = 6
num_frets = 4

type EffM e = Eff (canvas :: CANVAS| e)
type Line = Tuple Point Point

transformation :: Dimensions -> Transformation
transformation {width: w, height: h} (Point p) = point
  ( alpha * p.x / (toNumber (num_strings - 1) ) + beta)
  ( alpha * p.y / (toNumber num_frets) + v_margin + toNumber font_size )
  where
    alpha = h * (1.0 - 2.0 * v_margin) - toNumber font_size
    beta = (w - alpha) * 0.5

paint_chord_diagram :: forall e. Fingering -> CanvasElement -> EffM e Unit
paint_chord_diagram fingering element = do
  dimensions <- getCanvasDimensions element
  ctx <- getContext2D element
  let f = transformation dimensions
  traverse_ (paint_line ctx) (map (bimap f f) lines)
    where
      Tuple x0 y0 = Tuple 0 0
      Tuple x1 y1 = Tuple (num_strings-1) num_frets
      lines = map (\y -> Tuple (point_int x0 y) (point_int x1 y)) (y0..y1)
          <> map (\x -> Tuple (point_int x y0) (point_int x y1)) (x0..x1)

paint_line :: forall e. Context2D -> Line -> EffM e Unit
paint_line ctx (Tuple (Point p1) (Point p2)) = void $
  strokePath ctx $
    moveTo ctx p1.x p1.y
    *> lineTo ctx p2.x p2.y
    *> closePath ctx
