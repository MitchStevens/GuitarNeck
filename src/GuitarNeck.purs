module GuitarNeck where

import Control.Monad.Eff
import Data.Tuple
import Graphics.Canvas
import Partial.Unsafe
import Prelude

import Control.Monad.Aff (Aff)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (pow)

width  = 500
height = 120

type Point = Unit
type Chord = Unit
type State = Array (Tuple Point Chord)

type L e = Eff (canvas :: CANVAS | e)

data Query a
  = PaintNeck a
  | WipeNeck a
  | ClearAll a
  | MouseMove Point a
  | SetChord Chord a
  | GetChord (Chord -> a)

data Message = Toggled Boolean

guitar_neck :: forall eff. H.Component HH.HTML Query Unit Message (Aff (canvas :: CANVAS | eff))
guitar_neck =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = [Tuple unit unit]

  render :: State -> H.ComponentHTML Query
  render state =
    let
      pi = 3.14159365--label = if state then "On" else "Off"
    in
      HH.div_
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

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (canvas :: CANVAS | eff))
  eval = case _ of
    PaintNeck next -> do
      _ <- H.liftEff init_neck
      pure next
    WipeNeck next -> pure next
    ClearAll next -> pure next
    MouseMove _ next -> pure next 
    SetChord _ next -> pure next
    GetChord chord -> pure $ chord unit

init_neck :: forall e. Eff (canvas :: CANVAS | e) Unit
init_neck = void $ unsafePartial do
  Just canvas <- getCanvasElementById "guitar_neck"
  ctx <- getContext2D canvas
  _ <- setFillStyle "#edc889" ctx
  _ <- fillRect ctx {x:0.0, y:0.0, w:500.0, h:120.0}
  paint_fret ctx 5

fret_x :: Number -> Number
fret_x x = toNumber width * ((1.0 - pow t x)/(1.0 - t))
  where t = pow 2.0 (1.0/12.0)

paint_fret :: Context2D -> Int -> forall e. Eff (canvas :: CANVAS | e) Unit
paint_fret ctx fret_num = void $ unsafePartial do
  _ <- setFillStyle "#000000" ctx
  fillRect ctx
    { x: 0.0 --fret_x (toNumber fret_num)
    , y: 0.0
    , w: 2.0
    , h: toNumber height }
