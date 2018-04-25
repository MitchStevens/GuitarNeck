module UI.Queue where

import Data.Maybe
import Partial.Unsafe
import Prelude

import Data.Traversable (traverse)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (object)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import LimitQueue as Q

type Slot = Int
type ChildType g i o m = H.ParentHTML (Query i o) g Slot m

data Query i o a
  = Cons i a
  | Unsnoc a
  | ChildMessage o a

type Input g i o m =
  { limit :: Int
  , component :: H.Component HH.HTML g i o m
  }

type Message = Void

type State g i o m =
  { queue :: Q.LimitQueue (ChildType g i o m)
  , component :: H.Component HH.HTML g i o m
  , acc :: Int }

ui_queue :: forall g i o m. H.Component HH.HTML (Query i o) (Input g i o m) o m
ui_queue = H.parentComponent
  { initialState: initial
  , render
  , eval
  , receiver: const Nothing
  }
  where

  initial :: Input g i o m -> State g i o m
  initial input =
    { queue: unsafePartial (Q.empty input.limit)
    , component: input.component
    , acc: 0
    }

  render :: State g i o m -> H.ParentHTML (Query i o) g Slot m
  render state =
      HH.span
        [ HP.class_ (ClassName "queue-ui") ]
        (Q.toArray state.queue)

  eval :: Query i o ~> H.ParentDSL (State g i o m) (Query i o) g Slot o m
  eval = case _ of
    Cons input a -> do
      state <- H.get
      let child = HH.slot state.acc state.component input (HE.input ChildMessage)
      H.put $ state { queue = Q.insert child state.queue, acc = state.acc + 1 }
      pure a
    Unsnoc a -> pure a
    ChildMessage output a -> pure a

