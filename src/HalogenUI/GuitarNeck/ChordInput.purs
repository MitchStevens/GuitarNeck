module UI.ChordInput where

import Control.Monad.Eff
import Data.Either
import Data.Maybe
import Music
import Prelude
import Debug.Trace

import Control.Monad.Aff (Aff)
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)
import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Parser (parse_chord)
import Text.Parsing.Parser (ParseError(..), runParser, parseErrorMessage)
import Text.Parsing.Parser.Pos

type State = Either ParseError Chord

data Query a
  = StringInput String a
  | CurrentChord (State -> a)

data Message
  = ChangedState State

chord_input :: forall e. H.Component HH.HTML Query Unit Message (Aff e)
chord_input =
  H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where

  initialState :: State
  initialState = runParser "" parse_chord

  render :: State -> H.ComponentHTML Query
  render state = 
    HH.div_
      [ HH.input [ HE.onValueInput $ HE.input StringInput ]
      , either display_error display_chord state
      ]
  
  eval :: Query ~> H.ComponentDSL State Query Message (Aff e)
  eval = case _ of
    StringInput str next -> do
      let state = runParser str parse_chord
      H.put state
      H.raise $ ChangedState state
      pure next
    CurrentChord f -> do
      state <- H.get
      pure $ f state

display_error :: forall p i. ParseError -> H.HTML p i
display_error error =
  HH.span
    [ HP.id_ "error_display" ]
    [ HH.text (parseErrorMessage error) ]

display_chord :: forall p i. Chord -> H.HTML p i
display_chord chord =
  HH.span [ HP.class_ (H.ClassName "chord-display") ]
    [ HH.span [ HP.class_ (H.ClassName "root") ] [ HH.text (m.root) ]
    , HH.span [ HP.class_ (H.ClassName "mode") ] [ HH.text (m.mode) ]
    , HH.span [ HP.class_ (H.ClassName "head-ext") ] [ HH.text (m.head_ext) ]
    , HH.span_ (map ext_html m.tail_ext)
    ]
  where
    m = markup chord

    ext_html :: String -> H.HTML p i
    ext_html ext = HH.sup
      [ HP.class_ (H.ClassName "extension")]
      [ HH.text $ ext ]