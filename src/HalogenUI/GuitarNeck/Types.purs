module UI.GuitarNeck.Types where

import Data.Either (Either)
import Data.Maybe (Maybe)

import Fingering
import DOM.Event.Types (MouseEvent)

import Music (Chord)
import NeckData (NeckData)
import Reader (FingeringCache)
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
  | MouseMove MouseEvent a
  | MouseEnter a
  | MouseLeave a
  | MouseClick a
  | SetChord (Either ParseError Chord) a
  | Display a

data Output
  = ClickedFingering Fingering
  | FocusedFingering Fingering