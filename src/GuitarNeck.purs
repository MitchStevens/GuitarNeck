module GuitarNeck where

import Prelude

import Chord
import NeckData
import CanvasOperations

import Control.Monad.Aff (Aff)
import Control.Monad.Eff
import Data.Foldable
import Data.Tuple
import Data.Array ((..))
import Data.Int (toNumber, ceil)
import Data.Maybe (Maybe(..), fromMaybe)
import Math (pow, pi)

import Graphics.Canvas
import DOM.Event.MouseEvent as ME
import DOM.Event.Types as Event
import Halogen as H
import Halogen.HTML
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

default_neck_data = {width: 600.0, height: 120.0, num_frets: 15}

data Query a
  = PaintNeck NeckData a
  | WipeNeck a
  | ClearAll a
  | MouseMove Event.MouseEvent a
  | SetChord ChordFingering a
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
  initialState =
    { neck_data: default_neck_data
    , chords: []
    , focused: Nothing
    }

  render :: State -> H.ComponentHTML Query
  render state =
    let
      width  = state.neck_data.width
      height = state.neck_data.height
      show_mouseevent me = "client: ("<> show (ME.clientX me) <>", "<> show (ME.clientY me) <>")"
    in
      HH.div_
        [ HH.div
          [ HP.id_ "both_canvases"
          -- MouseEvent -> Maybe (Query Unit)
          -- MouseMove -> a -> Maybe (Query Unit)
          ]
          [ HH.canvas
            [ HP.id_ "guitar_neck"
            , HP.width  $ ceil width
            , HP.height $ ceil height
            ]
          , HH.canvas
            [ HP.id_ "guitar_notes"
            , HP.width  $ ceil width
            , HP.height $ ceil height
            , HE.onMouseMove (HE.input MouseMove)
            ]
          ]
        , HH.div_
          [ HH.textarea []
          , HH.text "Cm7(b5)"
          ]
        ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (canvas :: CANVAS | e))
  eval = case _ of
    PaintNeck neck_data next -> do
      state <- H.get
      let new_state = state {neck_data = neck_data}
      H.liftEff (init_neck new_state)
      H.put new_state
      pure next
    WipeNeck next -> pure next
    ClearAll next -> pure next
    MouseMove me next -> do
      state <- H.get
      pure next
    SetChord chord next -> do
      state <- H.get
      H.liftEff (paint_chord state chord)
      pure next 
    -- GetChord chord -> pure $ chord unit