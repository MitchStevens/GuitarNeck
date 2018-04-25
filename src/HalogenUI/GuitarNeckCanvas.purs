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
import UI.FFTypes

import Data.Array (length, zipWith, (..))
import Data.Int (toNumber)
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
  ctx <- getContext2D canvas
  let num_frets = neck.num_frets
  let tuple = Tuple ctx neck
  setFillStyle neck_color ctx
    *> fillRect ctx {x:neck.x_offset, y:neck.y_offset, w:neck.width, h:neck.height}
    *> traverse_ (paint_fret tuple) (0..num_frets)
    *> paint_inlays tuple
    *> paint_strings tuple
    *> traverse_ (paint_number tuple) (1..neck.num_frets)

paint_fret :: forall e. Tuple Context2D NeckData
                     -> Int
                     -> EffM e Unit
paint_fret (Tuple ctx neck) fret = void $
  setFillStyle fret_color ctx
  *> fillRect ctx { x: p.x, y: p.y, w: fret_width, h: neck.height }
  where
    f = neck_transformation neck
    Point p = f (point (toNumber fret) (-0.5)) - (point 0.0 (fret_width * 0.5))

paint_inlays :: forall e. Tuple Context2D NeckData -> EffM e Unit
paint_inlays (Tuple ctx neck) = void $
  setFillStyle inlay_color ctx
  *> traverse_ single_inlay (map f inlay_points)
  where
    f = fret_transformation neck
    inlay_points = [point 12.0 0.5, point 12.0 4.5] <>
      map ((\x -> point x 2.5) <<< toNumber) [3, 5, 7, 9, 15, 17, 19, 21]

    single_inlay :: Point -> forall e. EffM e Unit
    single_inlay (Point p) = void $
      beginPath ctx >>= flip arc (circle { x: p.x, y: p.y, r: inlay_radius }) >>= fill

paint_strings :: forall e. Tuple Context2D NeckData -> EffM e Unit
paint_strings (Tuple ctx neck_data) =
  setFillStyle string_color ctx
    *> (sequence_ $ zipWith single_string (0..5) gauges)
  where
    gauges = [0.2540, 0.3302, 0.4318, 0.6604, 0.9144, 1.1684]
    fret_size = 50.0
    f = neck_transformation neck_data
    single_string n g =
      beginPath ctx
        *> fillRect ctx
          { x: p.x
          , y: p.y
          , w: neck_data.width
          , h: neck_data.height* g / fret_size }
      where Point p = f $ point_int 0 n

paint_number :: forall e. Tuple Context2D NeckData -> Int -> EffM e Unit
paint_number (Tuple ctx neck) num = void $ do
  _ <- setFont (show font_size <> "px Georgia") ctx
  _ <- setFillStyle white ctx
  { width: text_width } <- measureText ctx (show num)
  write_text (show num) text_width
    where
      write_text :: String -> Number -> EffM e Unit
      write_text str width = void $ fillText ctx str p.x (max 0.0 p.y)
        where
          f = fret_transformation neck
          Point p = f (point (toNumber num) 0.0) - point (width*0.5) neck.y_offset

paint_chord :: Tuple Context2D NeckData -> FingeringData -> forall e. EffM e Unit
paint_chord (Tuple ctx neck) fing = 
  traverse_ (paint_fingering ctx) (to_points neck fing.fingering)
  where
    paint_fingering :: Context2D -> Point -> forall e. EffM e Unit
    paint_fingering ctx (Point p) = void $ 
      beginPath ctx
        >>= setFillStyle chord_color
        >>= flip arc (circle { x: p.x, y: p.y, r: radius })
        >>= fill

    radius = 8.5

paint_centeroid :: forall e. Context2D -> FingeringData -> EffM e Unit
paint_centeroid ctx fing = do
  let Point p = fing.centeroid
  gradient <- createRadialGradient
    { x0: p.x, y0: p.y, r0: 0.0
    , x1: p.x, y1: p.y, r1: radius } ctx
    >>= addColorStop 0.0 chord_color
    >>= addColorStop 1.0 transparent
  void $ beginPath ctx
    >>= setGradientFillStyle gradient
    >>= flip arc (circle { x: p.x, y: p.y, r: radius })
    >>= fill
  where radius = 40.0

{-
type State =
  { neck_data :: NeckData
  , curr_chord :: Maybe Chord
  , curr_fingerings :: Array FingeringData
  , focused :: Maybe Int
  , fingering_cache :: FingeringCache }
-}
type A a = { neck_data :: NeckData, curr_fingerings :: Array FingeringData, focused :: Maybe Int | a}
display_neck :: forall a e. A a -> EffM e Unit
display_neck state = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_notes"
  ctx <- getContext2D canvas
  setGlobalAlpha ctx 0.5
    *> setFillStyle chord_color ctx
    *> (sequence $ zipWith (f ctx) state.curr_fingerings (0..(length state.curr_fingerings - 1)))
    *> setGlobalAlpha ctx 1.0
  where
    f :: Context2D -> FingeringData -> Int -> EffM e Unit
    f ctx fing n = if state.focused == Just n
      then paint_chord (Tuple ctx state.neck_data) fing
      else paint_centeroid ctx fing

wipe_neck :: forall e. EffM e Unit
wipe_neck = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_notes"
  ctx <- getContext2D canvas
  { width: width, height: height } <- getCanvasDimensions canvas
  clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}

circle :: { x :: Number, y:: Number, r :: Number } -> Arc
circle c = { x: c.x, y: c.y, r: c.r, start: 0.0, end: 2.0 * pi}