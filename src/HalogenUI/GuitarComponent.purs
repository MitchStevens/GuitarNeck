module UI.GuitarComponent where

import Data.Either
import Data.Maybe
import NeckData
import Prelude
import UI.FFTypes

import Control.Monad.Eff.Console (log)
import Data.Const (Const(..))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.ChordDiagram (chord_diagram)
import UI.ChordDiagram.Types (default_dimensions)
import UI.ChordDiagram.Types as CD
import UI.ChordInput as CI
import UI.GuitarNeck (guitar_neck)
import UI.GuitarNeck.Types as GN
import UI.Queue as Q

type ChildQuery = Coproduct3 GN.Query CI.Query (Q.Query CD.Input CD.Output)
type ChildSlot  = Either3 Unit Unit Unit

type Input = NeckData
type Output = Void
type State =
  { neck_data :: NeckData
  , num_fingerings :: Int
  }

data Query a
  = GuitarNeckOutput GN.Output a
  | ChordInputOutput CI.Message a
  | ChordQueueOutput (Q.Output CD.Output) a

guitar_component :: forall e. H.Component HH.HTML Query Input Output (AffM e)
guitar_component = H.parentComponent 
  { initialState: initial
  , render
  , eval
  , receiver: const Nothing }
  where

  initial :: Input -> State
  initial input = { neck_data: input, num_fingerings: 0 }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (AffM e)
  render state = 
    HH.div [ HP.id_ "guitar" ]
      [ HH.slot' CP.cp1 unit guitar_neck
          state.neck_data (HE.input GuitarNeckOutput)
      , HH.slot' CP.cp2 unit CI.chord_input
          unit (HE.input ChordInputOutput)
      , HH.div_ [ HH.text (show state.num_fingerings) ]
      , HH.slot' CP.cp3 unit Q.ui_queue
          { limit: 3, component: chord_diagram } (HE.input ChordQueueOutput)
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (AffM e)
  eval = case _ of
    GuitarNeckOutput (GN.ClickedFingering fing) a -> do
      H.liftEff $ log (show fing)
      let x = { fingering: fing
              , name: "Chord"
              , dimensions: default_dimensions }
      _ <- H.query' CP.cp3 unit (Q.Cons x unit)
      pure a
    GuitarNeckOutput _ a -> pure a
    ChordInputOutput (CI.ChangedState state) a -> do
      _ <- H.query' CP.cp1 unit (GN.SetChord state unit)
      pure a
    ChordQueueOutput _ a -> pure a