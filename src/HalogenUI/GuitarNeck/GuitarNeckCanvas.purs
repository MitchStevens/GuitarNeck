module UI.GuitarNeckCanvas (
  paint_neck,
  display_neck,
  wipe_neck
) where

import Data.Maybe
import Data.Traversable
import Data.Tuple
import Fingering
import Fret
import Graphics.Canvas
import NeckData
import Partial.Unsafe
import Point
import Prelude
import UI.CanvasUtils
import UI.FFTypes

import Data.Array (foldM, length, zip, zipWith, (..))
import Data.Int (toNumber)
import Data.Lens ((^.))
import Debug.Trace (trace)
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
  Just canvas <- getCanvasElementById "guitar_neck"
  getContext2D canvas
  >>= setFillStyle neck_color
  >>= flip fillRect {x:neck.x_offset, y:neck.y_offset, w:neck.width, h:neck.height}
  >>= paint_frets
  >>= paint_inlays
  >>= paint_strings
  >>= paint_numbers
  where
  
  fret_t = fret_transformation neck
  neck_t = neck_transformation neck
  paint_frets ctx = foldM paint_fret ctx (0..neck.num_frets)

  paint_fret :: Context2D -> Int -> EffM e Context2D
  paint_fret ctx fret =
    setFillStyle fret_color ctx
    *> fillRect ctx { x: x, y: y, w: fret_width, h: neck.height }
    where
      Tuple x y = fret_t (Tuple (toNumber fret) (-0.5)) - (Tuple 0.0 (fret_width * 0.5))

  paint_inlays :: Context2D -> EffM e Context2D
  paint_inlays ctx =
    setFillStyle inlay_color ctx
    *> traverse_ single_inlay (map fret_t inlay_points)
    *> pure ctx
    where
      inlay_points = [Tuple 12.0 0.5, Tuple 12.0 4.5] <>
        map ((flip Tuple 2.5) <<< toNumber) [3, 5, 7, 9, 15, 17, 19, 21]

      single_inlay :: Point -> EffM e Unit
      single_inlay p = void $
        beginPath ctx >>= flip arc (circle { x: p^._x, y: p^._y, r: inlay_radius }) >>= fill

  paint_strings :: Context2D -> EffM e Context2D
  paint_strings ctx = do
    void $ setFillStyle string_color ctx
    sequence_ $ zipWith single_string (0..5) gauges
    pure ctx
    where
      gauges = [0.2540, 0.3302, 0.4318, 0.6604, 0.9144, 1.1684]
      fret_size = 50.0
      single_string n g =
        beginPath ctx
          *> fillRect ctx
            { x: p^._x
            , y: p^._y
            , w: neck.width
            , h: neck.height* g / fret_size }
        where p = neck_t $ point_int 0 n


  paint_numbers :: Context2D -> EffM e Context2D
  paint_numbers ctx = foldM paint_number ctx (1..neck.num_frets)
    where
      paint_number :: Context2D -> Int -> EffM e Context2D
      paint_number ctx num = do
        { width: w } <- measureText ctx (show num)
        let Tuple x y = fret_t (Tuple (toNumber num) 0.0) - Tuple (w*0.5) neck.y_offset
        setFont (show font_size <> "px Georgia") ctx
          *> setFillStyle white ctx
          *> fillText ctx (show num) x (max 0.0 y)

type A a = { neck_data :: NeckData, curr_fingerings :: Array FingeringData, focused :: Maybe Int | a}
display_neck :: forall a e. A a -> EffM e Unit
display_neck state = void $ unsafePartial $ do
  Just canvas <- getCanvasElementById "guitar_notes"
  getContext2D canvas
  >>= flip setGlobalAlpha 0.5
  >>= setFillStyle chord_color
  >>= paint_fingerings
  >>= flip setGlobalAlpha 1.0
  where

  neck = state.neck_data
  fret_t = fret_transformation neck

  paint_fingerings :: Context2D -> EffM e Context2D
  paint_fingerings ctx = foldM (flip paint_fingering) ctx tuples
    where
      fingerings = state.curr_fingerings
      tuples = zip fingerings (0..(length state.curr_fingerings))

      paint_fingering :: Tuple FingeringData Int -> Context2D -> EffM e Context2D
      paint_fingering (Tuple fing n) = if state.focused == Just n
        then paint_chord fing
        else paint_centeroid fing

  paint_chord :: FingeringData -> Context2D -> EffM e Context2D
  paint_chord fing ctx = 
    foldM (flip paint_dot) ctx (map fret_t $ to_points fing.fingering)
    where
      paint_dot :: Point -> Context2D -> EffM e Context2D
      paint_dot (Tuple x y) = 
        beginPath
        >=> setFillStyle chord_color
        >=> flip arc (circle { x: x, y: y, r: radius })
        >=> fill

      radius = 8.5

  paint_centeroid :: FingeringData -> Context2D -> EffM e Context2D
  paint_centeroid fing ctx = do
    let Tuple x y = fing.centeroid
    gradient <- createRadialGradient
      { x0: x, y0: y, r0: 0.0
      , x1: x, y1: y, r1: radius } ctx
      >>= addColorStop 0.0 chord_color
      >>= addColorStop 1.0 transparent
    beginPath ctx
      >>= setGradientFillStyle gradient
      >>= flip arc (circle { x: x, y: y, r: radius })
      >>= fill
    where radius = 40.0

wipe_neck :: forall e. EffM e Unit
wipe_neck = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_notes"
  ctx <- getContext2D canvas
  { width: width, height: height } <- getCanvasDimensions canvas
  clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}