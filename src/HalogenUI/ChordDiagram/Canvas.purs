module UI.ChordDiagram.Canvas (
  paint_chord_diagram--,
  --wipe_chord_diagram
) where

import Control.Monad.Eff
import Data.Lens
import Data.Maybe
import Data.Tuple
import Fingering
import Fret
import Graphics.Canvas
import Interval
import Partial.Unsafe
import Point
import Prelude
import UI.CanvasUtils

import Data.Array (foldM, partition, (..))
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Foldable (minimum, traverse_)
import Data.Int (toNumber)
import Halogen.HTML (canvas)
import UI.ChordDiagram.Types (Input)

font_size   = 20  :: Int
chord_perc  = 0.6 :: Number
num_strings = 6   :: Int
num_frets   = 4   :: Int

type EffM e a = Eff (canvas :: CANVAS| e) a
type Line = Tuple Point Point

canvas_t :: Dimensions -> Transformation
canvas_t {width: w, height: h} (Tuple x y) = Tuple
  ( alpha * x / (toNumber (num_strings - 1) ) + h_margin)
  ( alpha * y / (toNumber num_frets) + v_margin + toNumber font_size )
  where
    h_margin = (w - alpha) / 2.0
    v_margin = (h - alpha - toNumber font_size) / 2.0
    alpha = w * chord_perc

dot_t :: Transformation
dot_t (Tuple x y) = Tuple (5.0 - y) (x - 0.5)

dot_shift_t :: Fingering -> Transformation
dot_shift_t fingering = 
  if m <= 1.0 then dot_t else rmap (_-(m-1.0)) <<< dot_t
  where
    min_fret = inf $ fret_interval fingering
    m = maybe 0.0 (toNumber <<< unfret) min_fret

paint_chord_diagram :: forall e. Input -> String -> EffM e Unit
paint_chord_diagram state id = void $ unsafePartial $ do
  Just canvas <- getCanvasElementById id
  let radius = 7.0
  let f = canvas_t dims
  --let g = (_ + point 0.0 0.5) <<< f <<< transpose :: Transformation
  getContext2D canvas
    >>= paint_grid f
    >>= paint_dots f radius
    where

    fingering = state.fingering
    dims      = state.dimensions

    paint_grid :: Transformation -> Context2D -> EffM e Context2D
    paint_grid f ctx = foldM paint_line ctx (map (bimap f f) lines)
      where
        Tuple x0 y0 = Tuple 0 0
        Tuple x1 y1 = Tuple (num_strings-1) num_frets
        lines = map hrline (y0..y1) <> map vrline (x0..x1)
        hrline y = Tuple (point_int x0 y) (point_int x1 y)
        vrline x = Tuple (point_int x y0) (point_int x y1)

    paint_dots :: Transformation -> Number -> Context2D -> EffM e Context2D
    paint_dots f r ctx = foldM (paint_note_open r)    ctx (abs_pos open)
                      *> foldM (paint_note_fretted r) ctx (abs_pos fretted)
      where
        {yes: open, no: fretted} = partition ((eq 0.0) <<< fst) $ to_points fingering
        abs_pos = map (f <<< dot_shift_t fingering) :: Array Point -> Array Point

    paint_line :: Context2D -> Line -> EffM e Context2D
    paint_line ctx (Tuple p q) = 
      strokePath ctx $
        moveTo ctx (p^._x) (p^._y)
        *> lineTo ctx (q^._x) (q^._y)
        *> closePath ctx

    paint_note_fretted :: Number -> Context2D -> Point -> EffM e Context2D
    paint_note_fretted r ctx (Tuple x y) =
      beginPath ctx
      *> arc ctx (circle { x: x, y: y, r: r })
      *> fill ctx

    paint_note_open :: Number -> Context2D -> Point -> EffM e Context2D
    paint_note_open r ctx (Tuple x y) =
      beginPath ctx
      *> arc ctx (circle { x: x, y: y, r: r })
      *> stroke ctx

