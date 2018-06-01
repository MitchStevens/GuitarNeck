module UI.ChordDiagram.Types where

import Prelude
import Fingering
import Graphics.Canvas (Dimensions)

data Query a = Redraw a
type Input =
  { fingering :: Fingering
  , name :: String
  , dimensions :: Dimensions }
type Output = Void
type State = Input

default_dimensions :: Dimensions
default_dimensions = {width: 150.0, height: 150.0}