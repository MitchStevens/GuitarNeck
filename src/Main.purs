module Main where

import Graphics.Canvas
import GuitarNeck
import Chord
import Prelude

import Data.Tuple

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.HTML.Properties as H
import Halogen.VDom.Driver (runUI)

test_chord =
  [ Tuple B3 5
  , Tuple G3 5
  , Tuple D3 5
  , Tuple A2 3
  ]

main :: Eff (HA.HalogenEffects (canvas :: CANVAS)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI guitar_neck unit body
  io.query (PaintNeck initial_neck_data unit)
  io.query (SetChord test_chord unit)