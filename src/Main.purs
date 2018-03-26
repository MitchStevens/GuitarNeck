module Main where

import Data.Maybe
import Data.Tuple
import Fret
import Graphics.Canvas
import Reader
import Prelude

import UI.GuitarNeck

import DOM (DOM)
import DOM.HTML.Types
import DOM.Node.ParentNode (QuerySelector(..), querySelector)

import Control.Monad.Aff
import Control.Monad.Eff (Eff)

import Halogen.Aff (awaitLoad)
import Halogen.Aff as HA
import Halogen.Aff.Util
import Halogen.HTML.Properties as H
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax
import Node.FS

default_neck_data = {x_offset: 0.0, y_offset: 20.0, width: 600.0, height: 120.0, num_frets: 15}

main :: forall e. Eff (HA.HalogenEffects (ajax :: AJAX, fs :: FS, canvas :: CANVAS | e)) Unit
main = void $ HA.runHalogenAff do
  body <- await_guitar
  io <- runUI (guitar_neck default_neck_data) unit body
  io.query (PaintNeck unit)

await_guitar :: forall e. Aff (dom :: DOM | e) HTMLElement
await_guitar = do
  awaitLoad
  element <- selectElement (QuerySelector "#content #guitar")
  maybe (throwError (error "Could not find element")) pure element