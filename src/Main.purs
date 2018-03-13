module Main where

import Graphics.Canvas
import GuitarNeck
import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.HTML.Properties as H
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects (canvas :: CANVAS)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI guitar_neck unit body
  io.query (PaintNeck unit)