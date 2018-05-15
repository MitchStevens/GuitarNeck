module UI.GuitarNeck where

import DOM
import DOM.HTML
import DOM.HTML.HTMLElement
import DOM.HTML.Location
import DOM.HTML.Types
import DOM.HTML.Window

import Fingering
import Fret
import Graphics.Canvas
import Music
import NeckData
import Network.HTTP.Affjax
import Node.FS
import Point
import Prelude
import Reader
import UI.FFTypes
import UI.GuitarNeckCanvas

import Control.Apply (lift2)
import Control.Biapplicative (bipure)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log)

import Data.Array (index, length, mapMaybe, (..))
import Data.Either
import Data.Foldable
import Data.Int (toNumber, ceil)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.StrMap
import Debug.Trace
import Data.Tuple

import DOM.Event.MouseEvent as ME
import DOM.Event.Types as Event
import DOM.Node.ParentNode (QuerySelector(..))

import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Text.Parsing.Parser (ParseError)

type Input = NeckData

type State =
  { neck_data :: NeckData
  , curr_chord :: Maybe Chord
  , curr_fingerings :: Array FingeringData
  , focused :: Maybe Int
  , fingering_cache :: FingeringCache }

data Query a
  = PaintNeck a
  | WipeNeck a
  | ClearAll a
  | MouseMove Event.MouseEvent a
  | MouseEnter a
  | MouseLeave a
  | MouseClick a
  | SetChord (Either ParseError Chord) a
  | Display a

data Message
  = ClickedFingering Fingering
  | FocusedFingering Fingering

guitar_neck :: forall e. H.Component HH.HTML Query Input Message (AffM e)
guitar_neck =
  H.component
    { initialState: initial
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initial :: Input -> State
  initial neck =
    { neck_data: neck
    , curr_chord: Nothing
    , curr_fingerings: []
    , focused: Nothing
    , fingering_cache: { open: empty, moveable: empty }
    }

  render :: State -> H.ComponentHTML Query
  render state =
    let
      width  = state.neck_data.width  + state.neck_data.x_offset
      height = state.neck_data.height + state.neck_data.y_offset
      num_fingerings = if isJust state.curr_chord
        then "Found "<>(show $ length state.curr_fingerings)<>" chords."
        else "Couldn't find any chord shapes."
    in
      HH.div
        [ HP.id_ "both-canvases" ]
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
          , HE.onClick      (HE.input_ MouseClick)
          ]
        ]

  eval :: Query ~> H.ComponentDSL State Query Message (AffM e)
  eval = case _ of
    PaintNeck a -> do
      state <- H.get
      url <- H.liftEff $ window >>= location >>= origin
      fingerings <- H.liftAff $ read_fingerings url
      H.liftEff $ paint_neck state.neck_data
      H.put $ state { fingering_cache = fingerings }
      pure a
    WipeNeck a -> do
      H.liftEff wipe_neck
      pure a
    ClearAll a -> pure a
    MouseMove me a -> do
      state <- H.get
      element <- H.liftAff $ selectElement (QuerySelector "#content #guitar")
      offset <- H.liftEff $ maybe (pure zero) calc_offset element
      let p = point_int (ME.pageX me) (ME.pageY me) - offset
      let closest = closest_index state.neck_data p state.curr_fingerings
      H.liftEff $ log (show closest)
      if closest == state.focused
        then pure a
        else do
          maybe (pure unit) (H.raise <<< FocusedFingering) (focused_fingering state)
          H.put state { focused = closest }
          eval (Display a)
    MouseEnter a ->
      eval (WipeNeck a)
    MouseLeave a -> do
      H.modify (\st -> st { focused = Nothing })
      eval (Display a)
    MouseClick a -> do
      state <- H.get
      maybe (pure unit) (H.raise <<< ClickedFingering) (focused_fingering state)
      pure a
    SetChord eith a -> do
      state <- H.get
      next_state <- H.liftAff $ next_state eith state
      H.put next_state
      eval $ MouseLeave a
    Display a -> do
      state <- H.get
      void $ eval $ WipeNeck a
      H.liftEff $ display_neck state
      pure a

next_state :: forall e. Either ParseError Chord -> State -> AffM e State
next_state either state = case either of
  Left  _     -> pure $ state
    { curr_chord = Nothing
    , curr_fingerings = []
    , focused = Nothing }
  Right chord -> do
    let fingerings = get_fingerings state.fingering_cache chord
    let centeroids = mapMaybe (cache_centeroid state.neck_data) fingerings
    pure $ state
      { curr_chord = Just chord
      , curr_fingerings = centeroids
      , focused = Nothing }

calc_offset :: forall e. HTMLElement -> EffM e Point
calc_offset element = lift2 bipure (offsetLeft element) (offsetTop element)

focused_fingering :: State -> Maybe Fingering
focused_fingering state = do
  i <- state.focused
  fdata <- index state.curr_fingerings i
  pure fdata.fingering