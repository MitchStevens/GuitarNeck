module UI.ChordDiagram where

import Data.Maybe
import Music
import NeckData
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = Display Chord a | Wipe a
type Input = Unit
type Message = Void
type State = Unit

chord_diagram :: forall m. H.Component HH.HTML Query Input Message m
chord_diagram = H.component
  { initialState: initial
  , render
  , eval
  , receiver: const Nothing
  }
  where

  initial :: Input -> State
  initial = id

  render :: State -> H.ComponentHTML Query
  render state =
    let pi = 3.1415
    in
      HH.div_
        [ HH.text "CHORD\nDIAGRAM" ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Display _ a -> pure a
    Wipe a -> pure a
