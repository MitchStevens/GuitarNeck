module UI.GuitarNeck where

import Control.Apply (lift2)
import Control.Monad.Aff.AVar
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console

import Debug.Trace
import Fingering
import Fret
import Music
import NeckData
import Node.FS
import Text.Parsing.Parser (ParseError)
import Prelude
import Reader
import Point

import DOM
import DOM.HTML.HTMLElement
import DOM.HTML.Types
import DOM.Event.MouseEvent as ME
import DOM.Event.Types as Event
import DOM.Node.ParentNode (QuerySelector(..))

import Data.Array (length, (..), mapMaybe)
import Data.Either
import Data.Foldable
import Data.Int (toNumber, ceil)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.StrMap
import Data.Tuple

import Halogen (liftAff)
import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events as He
import Halogen.HTML.Properties as HP

import DOM.HTML
import DOM.HTML.Window
import DOM.HTML.Location

import Graphics.Canvas
import Network.HTTP.Affjax
import Math (pow, pi)
import UI.ChordInput as CI
import UI.GuitarNeckCanvas

data Slot = ChordInputSlot
derive instance eq_slot  :: Eq Slot
derive instance ord_slot :: Ord Slot

type State = 
  { neck_data :: NeckData
  , curr_chord :: Maybe Chord
  , curr_fingerings :: Array FingeringData
  , curr_focused :: Maybe Int
  , fingering_cache :: FingeringCache
  }

data Query a
  = PaintNeck a
  | SetNeck NeckData a
  | WipeNeck a
  | ClearAll a
  | MouseMove Event.MouseEvent a
  | MouseEnter a
  | MouseLeave a
  | SetChord Fingering a
  | ChordInputMessage CI.Message a

data Message = Unit
type AffM e = Aff
  ( ajax :: AJAX
  , avar :: AVAR
  , canvas :: CANVAS
  , console :: CONSOLE
  , dom :: DOM
  , fs :: FS
  , exception :: EXCEPTION
  , ref :: REF | e)

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
    , curr_chord: Nothing
    , curr_fingerings: []
    , curr_focused: Nothing
    , fingering_cache: { open: empty, moveable: empty }
    }

  render :: State -> H.ParentHTML Query CI.Query Slot (AffM e)
  render state =
    let
      width  = state.neck_data.width  + state.neck_data.x_offset
      height = state.neck_data.height + state.neck_data.y_offset
      num_fingerings = if isJust state.curr_chord
        then "Found "<>(show $ length state.curr_fingerings)<>" chords."
        else ""
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
            , HE.onMouseMove  (HE.input MouseMove)
            , HE.onMouseEnter (HE.input_ MouseEnter)
            , HE.onMouseLeave (HE.input_ MouseLeave)
            ]
          ]
        , HH.slot ChordInputSlot CI.chord_input unit (HE.input ChordInputMessage)
        , HH.div_ [ HH.text num_fingerings ]
        ]

  eval :: Query ~> H.ParentDSL State Query CI.Query Slot Void (AffM e)
  eval = case _ of
    PaintNeck next -> do
      state <- H.get
      url <- H.liftEff (window >>= location >>= origin)
      fingerings <- H.liftAff $ read_fingerings url
      H.liftEff (paint_neck state.neck_data)
      H.put $ state {fingering_cache = fingerings}
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
      let closest = get_closest state.neck_data p state.curr_fingerings
      case closest of
        Just c  -> eval (SetChord c.fingering next)
        Nothing -> pure next
    MouseEnter next -> do
      eval (WipeNeck next)
    MouseLeave next -> do
      state <- H.get
      _ <- eval (WipeNeck next)
      let centeroids = map (\x -> x.centeroid) state.curr_fingerings
      H.liftEff (paint_centeroids state.neck_data centeroids)
      pure next
    SetChord chord next -> do
      state <- H.get
      _ <- eval (WipeNeck next)
      H.liftEff (paint_chord state.neck_data chord)
      pure next
    ChordInputMessage (CI.ChangedState message) next -> do
      state <- H.get
      new_state <- H.liftAff $ next_state message state
      H.put new_state
      _ <- eval (MouseLeave next)
      pure next

next_state :: forall e. Either ParseError Chord -> State -> AffM e State
next_state either state = case either of
  Left  _     -> pure $ state
    { curr_chord = Nothing
    , curr_fingerings = []
    , curr_focused = Nothing }
  Right chord -> do
    let fingerings = get_fingerings state.fingering_cache chord
    let centeroids = mapMaybe (cache_centeroid state.neck_data) fingerings
    pure $ state
      { curr_chord = Just chord
      , curr_fingerings = centeroids
      , curr_focused = Nothing }

calc_offset :: forall e. HTMLElement -> Eff (dom :: DOM | e) Point
calc_offset element = lift2 point (offsetLeft element) (offsetTop element)