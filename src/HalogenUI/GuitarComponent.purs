module UI.GuitarComponent where

import Data.Maybe
import NeckData
import Prelude
import UI.FFTypes

import Data.Const (Const(..))
import Data.Either.Nested (Either3)
import Data.Either
import Data.Functor.Coproduct.Nested (Coproduct3)

import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import UI.ChordDiagram as CD
import UI.ChordDiagramQueue as CQ
import UI.ChordInput as CI
import UI.GuitarNeck as GN

type ChildQuery = Coproduct3 GN.Query CI.Query CQ.Query
type ChildSlot  = Either3 Unit Unit Unit

type Message = Void
type Input = NeckData
type State =
  { neck_data :: NeckData
  , num_fingerings :: Int
  }

data Query a
  = GuitarNeckMessage GN.Message a
  | ChordInputMessage CI.Message a
  | ChordQueueMessage CD.Message a
  | Initialise a

guitar_component :: forall e. H.Component HH.HTML Query Input Message (AffM e)
guitar_component = H.parentComponent
  { initialState: initial
  , render
  , eval
  , receiver: const Nothing
  }
  where

  initial :: Input -> State
  initial input = { neck_data: input, num_fingerings: 0 }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (AffM e)
  render state = 
    HH.div [ HP.id_ "guitar" ]
      [ HH.slot'
        CP.cp1 unit GN.guitar_neck
        state.neck_data (HE.input GuitarNeckMessage)
      , HH.slot'
        CP.cp2 unit CI.chord_input
        unit (HE.input ChordInputMessage)
      , HH.div_ [ HH.text (show state.num_fingerings) ]
      , HH.slot'
        CP.cp3 unit CQ.chord_diagram_queue
        { limit: 3, component: CD.chord_diagram } (HE.input ChordQueueMessage)
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (AffM e)
  eval = case _ of
    GuitarNeckMessage m a -> pure a
    ChordInputMessage (CI.ChangedState state) a -> do
      _ <- H.query' CP.cp1 unit (GN.SetChord state unit)
      pure a
    ChordQueueMessage m a -> pure a
    Initialise a -> do
      _ <- H.query' CP.cp1 unit (GN.PaintNeck unit)
      pure a
      
