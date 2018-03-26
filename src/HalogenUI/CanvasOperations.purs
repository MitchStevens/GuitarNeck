module CanvasOperations (
  paint_neck,
  paint_chord,
  wipe_neck
) where

import Fingering
import Control.Monad.Eff
import Fret
import Graphics.Canvas
import NeckData
import Partial.Unsafe
import Prelude

import Data.Maybe
import Data.Traversable
import Data.Tuple
import Data.Array ((..), zipWith)
import Data.Int (toNumber)
import Math (pow, pi)

type HexColor = String
neck_color   = "#edc889" :: HexColor
fret_color   = "#000000" :: HexColor
inlay_color  = "#dddddd" :: HexColor
string_color = "#dddddd" :: HexColor
chord_color  = "#ee1122" :: HexColor

fret_width = 2.0 :: Number
inlay_radius = 5.0 :: Number

paint_neck :: forall e. NeckData -> Eff (canvas :: CANVAS | e) Unit
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
                     -> Eff (canvas :: CANVAS | e) Unit
paint_fret (Tuple ctx neck) fret = void $
  setFillStyle fret_color ctx
  *> fillRect ctx
      { x: fret_x neck (toNumber fret) - (fret_width * 0.5)
      , y: neck.y_offset
      , w: fret_width
      , h: neck.height }

paint_inlays :: forall e. Tuple Context2D NeckData -> Eff (canvas :: CANVAS | e) Unit
paint_inlays (Tuple ctx neck) = void $
  setFillStyle inlay_color ctx
  *> traverse_ single_inlay inlay_points
  where
    x = fret_marker neck (Fret 12)
    y = str_y neck
    middle_y = y 2.5
    inlay_points = [point x (y 0.5), point x (y 4.5)] <>
      map (\x -> point (fret_marker neck (Fret x)) (y 2.5)) [3, 5, 7, 9, 15, 17, 19, 21]

    single_inlay :: Point -> forall e. Eff (canvas :: CANVAS | e) Unit
    single_inlay (Point p) = void $
      beginPath ctx >>= flip arc (circle { x: p.x, y: p.y, r: inlay_radius }) >>= fill

paint_strings :: forall e. Tuple Context2D NeckData -> Eff (canvas :: CANVAS | e) Unit
paint_strings (Tuple ctx neck) = 
  setFillStyle string_color ctx
    *> (sequence_ $ zipWith single_string (0..5) gauges)
  where
    gauges = [0.2540, 0.3302, 0.4318, 0.6604, 0.9144, 1.1684]
    fret_size = 50.0
    single_string n g =
      beginPath ctx
      *> fillRect ctx
        { x: neck.x_offset
        , y: str_y neck (toNumber n)
        , w: neck.width
        , h: neck.height* g / fret_size }

paint_number :: Tuple Context2D NeckData -> Int -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_number (Tuple ctx neck) num = void $ do
  _ <- setFont (show font_size <> "px Georgia") ctx
  _ <- setFillStyle "#000000" ctx
  { width: text_width } <- measureText ctx (show num)
  fillText ctx (show num) (fret_marker neck (Fret num) - (text_width*0.5)) y_pos
    where
      font_size = 14
      y_pos = max (toNumber font_size) (str_y neck 0.0 - neck.y_offset)

paint_chord :: NeckData -> Fingering -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_chord neck fingering = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_notes"
  ctx <- getContext2D canvas
  setGlobalAlpha ctx 0.35
    *> traverse_ (paint_fingering ctx) (to_points neck fingering)
    *> setGlobalAlpha ctx 1.0
  where
    paint_fingering :: Context2D -> Point -> forall e. Eff (canvas :: CANVAS | e) Unit
    paint_fingering ctx (Point p) = void $ 
      setFillStyle chord_color ctx
        >>= beginPath
        >>= flip arc (circle { x: p.x, y: p.y, r: radius })
        >>= fill

    radius = 8.5

wipe_neck :: forall e. Eff (canvas :: CANVAS | e) Unit
wipe_neck = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_notes"
  ctx <- getContext2D canvas
  { width: width, height: height } <- getCanvasDimensions canvas
  clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}

circle :: { x :: Number, y:: Number, r :: Number } -> Arc
circle c = { x: c.x, y: c.y, r: c.r, start: 0.0, end: 2.0 * pi}