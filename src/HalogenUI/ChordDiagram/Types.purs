module UI.ChordDiagram.Types where

import Prelude
import Fingering
import Graphics.Canvas (Dimensions)

data Query a = Redraw a
type Input =
  { fingering :: Fingering
  , dimensions :: Dimensions }
type Output = Void
type State = Input