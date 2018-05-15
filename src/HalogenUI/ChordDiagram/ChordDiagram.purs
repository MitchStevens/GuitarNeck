module UI.ChordDiagram where

import Data.Maybe
import Fingering
import Graphics.Canvas
import Halogen.Query
import Music
import NeckData
import Prelude
import UI.ChordDiagram.Canvas
import UI.ChordDiagram.Types
import UI.FFTypes

import Data.Int (ceil)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

chord_diagram :: forall e. H.Component HH.HTML Query Input Output (AffM e)
chord_diagram = H.lifecycleComponent
  { initialState: initial
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (action Redraw)
  , finalizer: Nothing
  }
  where

  initial :: Input -> State
  initial = id

  render :: State -> H.ComponentHTML Query
  render state =
    let pi = 3.1415
    in HH.div
      [ HP.class_ (H.ClassName "chord-diagram") ]
      [ HH.canvas
        [ HP.id_ (show state.fingering)
        , HP.width  $ ceil state.dimensions.width
        , HP.height $ ceil state.dimensions.height ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Output (AffM e)
  eval = case _ of
    Redraw a -> do
      state <- H.get
      H.liftEff $ paint_chord_diagram state (show state.fingering)
      pure a

