module UI.GuitarNeck (
  guitar_neck
) where

import Data.Either
import Data.Foldable
import Data.StrMap
import Data.Tuple
import Debug.Trace
import Fingering
import Music
import Point
import Prelude
import Reader
import UI.FFTypes
import UI.GuitarNeck.Canvas
import UI.GuitarNeck.Types

import CSS as C
import Control.Apply (lift2)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log)
import DOM.Event.MouseEvent as ME
import DOM.HTML (window)
import DOM.HTML.HTMLElement (offsetLeft, offsetTop)
import DOM.HTML.Location (origin)
import DOM.HTML.Types (HTMLElement)
import DOM.HTML.Window (location)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Array (index, length, mapMaybe, (..))
import Data.Int (toNumber, ceil)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.HTML (text)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Svg.Attributes (viewBox)
import Svg.Elements (svg)
import Text.Parsing.Parser (ParseError)
import UI.GuitarNeck.DrawSvg (draw_neck)

guitar_neck :: forall e. H.Component HH.HTML Query Input Output (AffM e)
guitar_neck = H.lifecycleComponent
  { initialState: initial
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (PaintNeck unit)
  , finalizer: Nothing
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
      width  = state.neck_data.width
      height = state.neck_data.height
    in
      HH.div
        [ HP.id_ "guitar-neck"
        , HE.onMouseMove  (HE.input MouseMove)
        , HE.onMouseEnter (HE.input_ MouseEnter)
        , HE.onMouseLeave (HE.input_ MouseLeave)
        , HE.onClick      (HE.input_ MouseClick)
        ]
        [ draw_neck state.neck_data ]

  eval :: Query ~> H.ComponentDSL State Query Output (AffM e)
  eval = case _ of
    PaintNeck a -> do
      --state <- H.get
      --url <- H.liftEff $ window >>= location -->>= origin
      --fingerings <- H.liftAff $ read_fingerings --url
      --H.liftEff $ paint_neck state.neck_data
      --H.put $ state { fingering_cache = --fingerings }
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
calc_offset element = lift2 Tuple (offsetLeft element) (offsetTop element)

focused_fingering :: State -> Maybe Fingering
focused_fingering state = do
  i <- state.focused
  fdata <- index state.curr_fingerings i
  pure fdata.fingering