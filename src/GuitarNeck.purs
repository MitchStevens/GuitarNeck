module GuitarNeck where

import Chord
import Control.Monad.Eff
import Data.Foldable
import Data.Tuple
import Graphics.Canvas
import Partial.Unsafe
import Prelude

import Control.Monad.Aff (Aff)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HHC
import Math (pow, pi)

width     =  600
height    =  120
num_frets =  15

type NeckData =
  { width  :: Number
  , height :: Number
  , num_frets :: Int
  }

initial_neck_data = {width: 600.0, height: 120.0, num_frets: 15}

type State = Array (Tuple Point Chord)

data Query a
  = PaintNeck a
  | WipeNeck a
  | ClearAll a
  | MouseMove Point a
  | SetChord Chord a
  -- | GetChord (Chord -> a)

data Message = Toggled Boolean

guitar_neck :: forall e. H.Component HH.HTML Query Unit Message (Aff (canvas :: CANVAS | e))
guitar_neck =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = []

  render :: State -> H.ComponentHTML Query
  render state =
    let
      pi = 3.14159365
    in
      HH.div_
        [ HH.div
          [ HP.id_ "both_canvases"
          , HHC.style $ "width: " <> show width <> "px"
          ]
          [ HH.canvas
            [ HP.id_ "guitar_neck"
            , HP.width  width
            , HP.height height
            ]
          , HH.canvas
            [ HP.id_ "guitar_notes"
            , HP.width  width
            , HP.height height
            ]
          ]
          , HH.select_ [HH.option_ [HH.text "example1"]]
        ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (canvas :: CANVAS | e))
  eval = case _ of
    PaintNeck next ->
      H.liftEff (init_neck initial_neck_data) *> pure next
    WipeNeck next -> pure next
    ClearAll next -> pure next
    MouseMove _ next -> pure next
    SetChord chord next ->
      H.liftEff (paint_chord chord) *> pure next 
    -- GetChord chord -> pure $ chord unit

init_neck :: NeckData -> forall e. Eff (canvas :: CANVAS | e) Unit
init_neck neck_data = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_neck"
  ctx <- getContext2D canvas
  setFillStyle "#edc889" ctx
    *> fillRect ctx {x:0.0, y:0.0, w:neck_data.width, h:neck_data.height}
    *> traverse_ (paint_fret ctx) (0..num_frets)
    *> paint_inlays ctx
    *> paint_strings ctx

fret_x :: Number -> Number
fret_x x = toNumber width * (1.0 - pow t x) / (1.0 - pow t (toNumber num_frets))
  where t = pow 2.0 (-1.0/12.0)

str_y :: Number -> Number
str_y y = (y * 2.0 + 1.0) * toNumber height / 12.0

between_fret :: Int -> Number
between_fret fret = (fret_x n + fret_x (n-1.0)) * 0.5
  where n = toNumber fret

paint_fret :: Context2D -> Int -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_fret ctx fret_num = void $
  setFillStyle "#000000" ctx
  *> fillRect ctx
      { x: fret_x (toNumber fret_num)
      , y: 0.0
      , w: 2.0
      , h: toNumber height }

paint_inlays :: Context2D -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_inlays ctx = void $
  setFillStyle "#dddddd" ctx
  *> traverse_ single_inlay [3, 5, 7, 9, 15, 17, 19, 21]
  *> beginPath ctx *> arc ctx (circle { x: _x, y: _y * 1.0, r: radius }) *> fill ctx
  *> beginPath ctx *> arc ctx (circle { x: _x, y: _y * 5.0, r: radius }) *> fill ctx
  where
    _x = between_fret 12
    _y = toNumber height / 6.0
    radius = 5.0
    single_inlay fret = beginPath ctx *> arc ctx circ *> fill ctx
      where circ = circle { x: between_fret fret, y: (toNumber height) * 0.5, r: radius }

paint_strings :: Context2D -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_strings ctx = void $
  setFillStyle "#dddddd" ctx *> traverse_ single_string (0..5)
  where
    single_string n =
      beginPath ctx
      *> fillRect ctx
        { x: 0.0
        , y: str_y (toNumber n)
        , w: toNumber width
        , h: 1.0 }

paint_chord :: Chord -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_chord chord = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_notes"
  ctx <- getContext2D canvas
  setFillStyle "#dd2211" ctx *> traverse_ (paint_fingering ctx) chord
  where
    paint_fingering ctx (Tuple str_num fret_num) =
      beginPath ctx
        *> arc ctx (circle { x: between_fret fret_num, y: str_y (str_to_num str_num), r: 10.0 })
        *> fill ctx

circle :: { x :: Number, y:: Number, r :: Number } -> Arc
circle c = { x: c.x, y: c.y, r: c.r, start: 0.0, end: 2.0 * pi}