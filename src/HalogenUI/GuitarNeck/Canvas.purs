module UI.GuitarNeck.Canvas (
  paint_neck,
  display_neck,
  wipe_neck
) where

import Data.Maybe
import Data.Traversable
import Data.Tuple
import Fingering
import Fret
import NeckData
import Partial.Unsafe
import Point
import Prelude
import UI.CanvasUtils
import UI.FFTypes
import UI.GuitarNeck.Types

import Data.Array (foldM, length, zip, zipWith, (..))
import Data.Int (toNumber)
import Data.Lens ((^.))
import Debug.Trace (trace)
import Graphics.Canvas (clearRect, getCanvasDimensions, getCanvasElementById, getContext2D)
import Graphics.Canvas.Free (Graphics, arc, beginPath, fill, fillRect, fillText, measureText, runGraphics, setAlpha, setFillStyle, setFont, stroke)
import Math (pi)

type HexColor = String
neck_color   = "#edc889" :: HexColor
fret_color   = "#000000" :: HexColor
inlay_color  = "#dddddd" :: HexColor
string_color = "#dddddd" :: HexColor
chord_color  = "#ee1122" :: HexColor
white         = "#FFFFFF" :: HexColor
transparent   = "transparent" :: HexColor

fret_width = 2.0 :: Number
inlay_radius = 5.0 :: Number
font_size = 14 :: Int


paint_neck :: forall e. NeckData -> EffM e Unit
paint_neck neck = void $ unsafePartial $ do
  Just canvas <- getCanvasElementById "guitar-neck"
  ctx <- getContext2D canvas
  runGraphics ctx do
    paint_fretboard
    paint_frets
    paint_inlays
    paint_strings
    paint_numbers
  where
  
  fret_t = fret_transformation neck
  neck_t = neck_transformation neck
  paint_frets = traverse_ paint_fret (0..neck.num_frets)

  paint_fretboard :: Graphics Unit
  paint_fretboard = do
    setFillStyle neck_color
    fillRect {x:neck.x_offset, y:neck.y_offset, w:neck.width, h:neck.height}

  paint_fret :: Int -> Graphics Unit
  paint_fret fret = do
    setFillStyle fret_color
    fillRect { x: x, y: y, w: fret_width, h: neck.height }
    where
      Tuple x y = fret_t (Tuple (toNumber fret) (-0.5)) - (Tuple 0.0 (fret_width * 0.5))

  paint_inlays :: Graphics Unit
  paint_inlays = do
    setFillStyle inlay_color
    traverse_ (single_inlay <<< fret_t) inlay_points
    where
      inlay_points = [Tuple 12.0 0.5, Tuple 12.0 4.5] <>
        map ((flip Tuple 2.5) <<< toNumber) [3, 5, 7, 9, 15, 17, 19, 21]

      single_inlay :: Point -> Graphics Unit
      single_inlay (Tuple x y) =
        beginPath *> arc (circle { x: x, y: y, r: inlay_radius }) *> fill

  paint_strings :: Graphics Unit
  paint_strings = do
    setFillStyle string_color
    sequence_ $ zipWith single_string (0..5) gauges
    where
      gauges = [0.2540, 0.3302, 0.4318, 0.6604, 0.9144, 1.1684]
      fret_size = 50.0
      single_string n g =
        let Tuple x y = neck_t $ point_int 0 n
        in do
          beginPath
          fillRect
            { x: x
            , y: y
            , w: neck.width
            , h: neck.height* g / fret_size }
          stroke

  paint_numbers :: Graphics Unit
  paint_numbers = traverse_ paint_number (1..neck.num_frets)
    where
      paint_number :: Int -> Graphics Unit
      paint_number num = do
        { width: w } <- measureText (show num)
        let Tuple x y = fret_t (Tuple (toNumber num) 0.0) - Tuple (w*0.5) neck.y_offset
        setFont (show font_size <> "px Georgia")
        setFillStyle white
        fillText (show num) x (max 0.0 y)

type A a = { neck_data :: NeckData, curr_fingerings :: Array FingeringData, focused :: Maybe Int | a}
display_neck :: forall a e. A a -> EffM e Unit
display_neck state = void $ unsafePartial $ do
  Just canvas <- getCanvasElementById "guitar-notes"
  ctx <- getContext2D canvas
  runGraphics ctx do
    setFillStyle chord_color
    paint_fingerings
  where

  neck = state.neck_data
  fret_t = fret_transformation neck

  paint_fingerings :: Graphics Unit
  paint_fingerings =
    sequence_ $ zipWith paint_fingering fingerings (0..(length fingerings))
    where
      fingerings = state.curr_fingerings

      paint_fingering :: FingeringData -> Int -> Graphics Unit
      paint_fingering fing n =
        if state.focused == Just n
          then paint_chord fing
          else paint_centeroid fing

  paint_chord :: FingeringData -> Graphics Unit
  paint_chord fing = traverse_ paint_dot (map fret_t $ to_points fing.fingering)
    where
      paint_dot :: Point -> Graphics Unit
      paint_dot (Tuple x y) = do
        beginPath
        setFillStyle chord_color
        arc (circle {x: x, y: y, r: radius})
        fill

      radius = 8.5

  {-
  paint_centeroid :: FingeringData -> Graphics Unit
  paint_centeroid = do
    let Tuple x y = fing.centeroid
    radial -}

  paint_centeroid :: FingeringData -> Graphics Unit
  paint_centeroid fing = do
    let Tuple x y = fing.centeroid
    --gradient <- createRadialGradient
    --  { x0: x, y0: y, r0: 0.0
    --  , x1: x, y1: y, r1: radius } ctx
    --  >>= addColorStop 0.0 chord_color
    --  >>= addColorStop 1.0 transparent
    beginPath
    --  >>= setGradientFillStyle gradient
    arc (circle { x: x, y: y, r: radius })
    fill
    where radius = 40.0

wipe_neck :: forall e. EffM e Unit
wipe_neck = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar-notes"
  ctx <- getContext2D canvas
  { width: width, height: height } <- getCanvasDimensions canvas
  clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}