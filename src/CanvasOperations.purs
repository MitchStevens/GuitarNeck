module CanvasOperations (
  State,
  init_neck,
  paint_chord,
  wipe_neck
) where

import Prelude
import Fret
import Chord
import NeckData

import Control.Monad.Eff
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Math (pow, pi)
import Partial.Unsafe

import Graphics.Canvas

type State = 
  { neck_data :: NeckData
  , chords :: Array ChordData
  , focused :: Maybe Int
  }

neck_color   = "#edc889"
fret_color   = "#000000"
inlay_color  = "#dddddd"
string_color = "#dddddd"
chord_color  = "#ee1122"

init_neck :: NeckData -> forall e. Eff (canvas :: CANVAS | e) Unit
init_neck neck = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_neck"
  ctx <- getContext2D canvas
  let num_frets = neck.num_frets
  let tuple = Tuple ctx neck
  setFillStyle neck_color ctx
    *> fillRect ctx {x:0.0, y:0.0, w:neck.width, h:neck.height}
    *> traverse_ (paint_fret tuple) (0..num_frets)
    *> paint_inlays tuple
    *> paint_strings tuple

paint_fret :: Tuple Context2D NeckData -> Int -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_fret (Tuple ctx neck) fret = void $
  setFillStyle fret_color ctx
  *> fillRect ctx
      { x: fret_x neck (toNumber fret)
      , y: 0.0
      , w: 2.0
      , h: neck.height }

paint_inlays :: Tuple Context2D NeckData -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_inlays (Tuple ctx neck) = void $
  setFillStyle inlay_color ctx
  *> traverse_ single_inlay inlay_points
  where
    x = fret_marker neck (Fret 12)
    y = neck.height / 6.0
    radius = 5.0
    inlay_points = [point x y, point x (y*5.0)] <>
      map (\x -> point (fret_marker neck (Fret x)) (y*3.0)) [3, 5, 7, 9, 15, 17, 19, 21]

    single_inlay :: Point -> forall e. Eff (canvas :: CANVAS | e) Unit
    single_inlay (Point p) = void $
      beginPath ctx >>= flip arc (circle { x: p.x, y: p.y, r: radius }) >>= fill

paint_strings :: Tuple Context2D NeckData -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_strings (Tuple ctx neck) = void $
  setFillStyle string_color ctx *> traverse_ single_string (0..5)
  where
    single_string n =
      beginPath ctx
      *> fillRect ctx
        { x: 0.0
        , y: str_y neck (toNumber n)
        , w: neck.width
        , h: 1.0 }

paint_chord :: NeckData -> ChordData -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_chord neck chord_data = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_notes"
  ctx <- getContext2D canvas
  setGlobalAlpha ctx 0.35
    *> traverse_ (paint_fingering ctx) (to_points neck chord_data.fingering)
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