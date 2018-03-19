module UI.GuitarNeck where

import Network.HTTP.Affjax

import CanvasOperations
import ChordFingering
import Debug.Trace
import Fret
import Halogen.HTML
import NeckData
import Node.FS
import Prelude
import Reader
import Music

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Control.Apply (lift2)
import Control.Monad.Aff (Aff)

import DOM
import DOM.Event.MouseEvent as ME
import DOM.Event.Types as Event
import DOM.Node.ParentNode (QuerySelector(..))
import DOM.HTML.HTMLElement
import DOM.HTML.Types

import Data.Array ((..))
import Data.Int (toNumber, ceil)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Foldable
import Data.Tuple
import Math (pow, pi)

import Graphics.Canvas
import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import UI.ChordInput as CI

data Slot = ChordInputSlot
derive instance eq_slot  :: Eq Slot
derive instance ord_slot :: Ord Slot

type State = 
  { neck_data :: NeckData
  , chords :: Array FingeringData
  , focused :: Maybe Int
  }

data Query a
  = PaintNeck a
  | SetNeck NeckData a
  | WipeNeck a
  | ClearAll a
  | MouseMove Event.MouseEvent a
  | SetChord ChordFingering a
  | ChordInputMessage CI.Message a

data Message = Unit

type AffM e = Aff (ajax :: AJAX, canvas :: CANVAS, dom :: DOM, fs :: FS, exception :: EXCEPTION, ref :: REF | e)

guitar_neck :: forall e. NeckData -> H.Component HH.HTML Query Unit Void (AffM e)
guitar_neck neck =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { neck_data: neck
    , chords: []
    , focused: Nothing
    }

  render :: State -> H.ParentHTML Query CI.Query Slot (AffM e)
  render state =
    let
      width  = state.neck_data.width
      height = state.neck_data.height
    in
      HH.div_
        [ HH.div
          [ HP.id_ "both-canvases"
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
        , HH.slot ChordInputSlot CI.chord_input unit (HE.input ChordInputMessage)
        ]

  eval :: Query ~> H.ParentDSL State Query CI.Query Slot Void (AffM e)
  eval = case _ of
    PaintNeck next -> do
      state <- H.get
      H.liftEff (paint_neck state.neck_data)
      pure next
    SetNeck neck_data next -> do
      state <- H.get
      let new_state = state {neck_data = neck_data}
      H.liftEff (paint_neck neck_data)
      H.put new_state
      pure next
    WipeNeck next -> do
      H.liftEff wipe_neck
      pure next
    ClearAll next -> pure next
    MouseMove me next -> do
      state <- H.get
      element <- H.liftAff $ selectElement (QuerySelector "#content #guitar")
      offset <- H.liftEff $ maybe (pure zero) calc_offset element
      let p = point (toNumber $ ME.pageX me) (toNumber $ ME.pageY me) - offset
      cmajs <- H.liftEff (get_fingerings state.neck_data chord)
      let closest = get_closest state.neck_data p cmajs
      case closest of
        Just c  -> eval (SetChord c.fingering next)
        Nothing -> pure next
      {-
        i <- index of closest chord
        if (state.focus != Just i) then
          state.focus = Just i
          SetChord chords i
      -}
    SetChord chord next -> do
      state <- H.get
      _ <- eval (WipeNeck next)
      H.liftEff (paint_chord state.neck_data chord)
      pure next 
    -- GetChord chord -> pure $ chord unit
    ChordInputMessage _ next -> do
      pure next


calc_offset :: forall e. HTMLElement -> Eff (dom :: DOM | e) Point
calc_offset element = lift2 point (offsetLeft element) (offsetTop element)

chord = Chord C Major [Add 7]