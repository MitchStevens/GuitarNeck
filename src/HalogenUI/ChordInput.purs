module UI.ChordInput where

import Control.Monad.Eff
import Data.Either
import Data.Maybe
import Music
import Prelude

import Control.Monad.Aff (Aff)
import Data.Traversable (traverse)
import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Parser (parse_chord)
import Text.Parsing.Parser (ParseError(..), runParser)

type State = Either String Chord

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
  initialState = Left "No Chord Yet"

  render :: State -> H.ComponentHTML Query
  render state = 
    HH.div_
      [ HH.textarea
        [ HE.onValueInput $ HE.input StringInput ]
      , either display_error display_chord state
      ]
  
  eval :: Query ~> H.ComponentDSL State Query Message (Aff e)
  eval = case _ of
    StringInput str next -> do
      H.put $ case runParser str parse_chord of
        Left  error -> Left $ show error
        Right chord -> Right chord
      pure next
    CurrentChord f -> do
      state <- H.get
      pure $ f state

display_error :: forall p i. String -> H.HTML p i
display_error error = HH.span [ HP.id_ "error_display" ] [ HH.text error ]

display_chord :: forall p i. Chord -> H.HTML p i
display_chord (Chord root mode exts) = 
  HH.span [ HP.id_ "chord-display" ]
    [ HH.span [ HP.class_ (H.ClassName "root") ] [ HH.text (show root)]
    , HH.span [ HP.class_ (H.ClassName "mode") ] [ HH.text (show mode)]
    , HH.span_ (map ext_html exts)
    ]
  where
    ext_html :: Extension -> H.HTML p i
    ext_html ext = HH.span
      [ HP.class_ (H.ClassName "extension")]
      [ HH.text (show  ext) ]
