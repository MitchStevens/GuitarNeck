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
import Graphics.Canvas.Free
import Interval
import Partial.Unsafe
import Point
import Prelude
import UI.CanvasUtils

import Data.Array (foldM, partition, (..))
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Foldable (minimum, traverse_)
import Data.Int (toNumber)
import Graphics.Canvas (CANVAS, Dimensions, Transform, getCanvasElementById, getContext2D)
import Halogen.HTML (canvas)
import UI.ChordDiagram.Types (Input, State)

font_size   = 20  :: Int    -- Size of the chord title ex. Cmaj7
chord_perc  = 0.6 :: Number -- Percentage of the width the chord diagram takes up
num_strings = 6   :: Int    -- Guitar strings = 6
num_frets   = 4   :: Int    -- Will very rarely need more than 4 frets
spacing     = 7.0 :: Number -- height between the title and the diagram

type Line = Tuple Point Point

dark_grey = "#111111" :: String
line_width = 1.0 :: Number

dot_t :: Transformation
dot_t (Tuple x y) = Tuple (5.0 - y) (x - 0.5)

as_t :: Transform -> Transformation
as_t t (Tuple x y) = Tuple
  (t.m11 * x + t.m31)
  (t.m22 * y + t.m32)

{-
  id: the id in the dom

-}
paint_chord_diagram :: forall e. State -> String -> Eff (canvas :: CANVAS | e) Unit
paint_chord_diagram state id = void $ unsafePartial $ do
  Just canvas <- getCanvasElementById id
  ctx <- getContext2D canvas
  runGraphics ctx $ do
    paint_grid
    paint_dots
    paint_title
    paint_location
  where
  radius = 6.0
  fingering             = state.fingering
  {width: w, height: h} = state.dimensions
  
  alpha = w * chord_perc -- The width and height of the diagram
  h_margin = (w - alpha) / 2.0
  v_margin = (h - alpha - toNumber font_size - spacing) / 2.0

  canvas_transform =
    { m11: alpha / (toNumber (num_strings - 1)), m12: 0.0
    , m21: 0.0, m22: alpha / (toNumber num_frets)
    , m31: h_margin, m32: v_margin + toNumber font_size + spacing }

  dot_shift_t :: Transformation
  dot_shift_t = 
    if m <= 1.0 then dot_t else rmap (_-(m-1.0)) <<< dot_t
    where
      min_fret = inf $ fret_interval fingering
      m = maybe 0.0 (toNumber <<< unfret) min_fret

  paint_grid :: Graphics Unit 
  paint_grid = do
    save
    transform canvas_transform
    setLineWidth 0.05
    traverse_ paint_line lines
    if not (is_open fingering)
      then pure unit
      else paint_nut
    restore
    where
      Tuple x0 y0 = Tuple 0 0
      Tuple x1 y1 = Tuple num_strings num_frets
      lines = map hrline (y0..y1) <> map vrline (x0..x1)
      hrline y = Tuple (point_int x0 y) (point_int (x1-1) y)
      vrline x = Tuple (point_int x y0) (point_int x y1)

      paint_nut = rect { x: 0.0, y: z, w: toNumber (num_frets-1), h: z }
        where z = 0.1

  paint_title :: Graphics Unit
  paint_title = do
    setFont (show font_size <> "px Georgia")
    { width: tw } <- measureText state.name
    setFillStyle dark_grey
    fillText state.name ((w - tw) / 2.0) (v_margin + (toNumber font_size)* 0.5)

  paint_location :: Graphics Unit
  paint_location = case inf (fret_interval fingering) of
    Nothing       -> pure unit
    Just (Fret n) -> if n <= 1 
      then pure unit
      else do
        setFont (show "9px Georgia")
        { width: tw } <- measureText (show n)
        setFillStyle dark_grey
        fillText (show n) (h_margin - tw - 2.0) (v_margin + (toNumber font_size) * 1.5 + spacing)

  paint_dots :: Graphics Unit
  paint_dots = do
    setLineWidth line_width
    traverse_ (paint_note_open    <<< abs_pos) open
    traverse_ (paint_note_fretted <<< abs_pos) fretted
    where
      {yes: open, no: fretted} = partition ((eq 0.0) <<< fst) $ to_points fingering
      abs_pos = (as_t canvas_transform) <<< dot_shift_t

  paint_line :: Line -> Graphics Unit
  paint_line (Tuple p q) = do
    stroke
    uncurry moveTo p
    uncurry lineTo q
    closePath

  paint_note_fretted :: Point -> Graphics Unit
  paint_note_fretted (Tuple x y) =
    beginPath *> arc (circle { x: x, y: y, r: radius }) *> fill

  paint_note_open :: Point -> Graphics Unit
  paint_note_open (Tuple x y) =
    beginPath *> arc (circle { x: x, y: y, r: radius }) *> stroke
