module Main where

import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff (Eff)

import DOM.HTML
import DOM.HTML.HTMLElement
import DOM.HTML.Location
import DOM.HTML.Types
import DOM.HTML.Window

import Data.Maybe
import Data.Tuple
import Debug.Trace
import Fingering
import Fret
import Graphics.Canvas
import Halogen.Aff.Util
import NeckData
import Network.HTTP.Affjax
import Node.FS
import Prelude
import Reader

import UI.ChordDiagram
import UI.FFTypes
import UI.GuitarComponent
import UI.Queue (Query(..), ui_queue)

import DOM (DOM)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import Halogen (action)
import Halogen as H
import Halogen.Aff (awaitLoad)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

default_neck_data :: NeckData
default_neck_data =
  { x_offset: 0.0
  , y_offset: 20.0
  , width: 600.0
  , height: 120.0
  , num_frets: 15 }

selector :: String
selector = "#content #guitar-component"

main :: forall e. EffM e Unit
main = void $ HA.runHalogenAff do
  element <- await_guitar
  neck_data <- liftEff $ calc_neck_data element
  io <- runUI ui_queue input element
  io.query (cons cmaj)
  io.query (cons amin)
  pure unit
  --io <- runUI guitar_component neck_data element
  where
    input =
      { limit: 5
      , component: chord_diagram }

    cmaj = Fingering
      { e4: Just (Fret 3)
      , b3: Just (Fret 5)
      , g3: Just (Fret 5)
      , d3: Just (Fret 5)
      , a2: Just (Fret 3)
      , e2: Nothing }
    
    amin = Fingering
      { e4: Just (Fret 0)
      , b3: Just (Fret 1)
      , g3: Just (Fret 2)
      , d3: Just (Fret 2)
      , a2: Just (Fret 0)
      , e2: Nothing }

    dims = {width: 150.0, height: 150.0}
    cons chord = Cons {fingering: chord, dimensions: dims} unit

await_guitar :: forall e. AffM e HTMLElement
await_guitar = do
  awaitLoad
  element <- selectElement $ QuerySelector selector
  maybe (throwError (error $ "Could not find element: " <> selector)) pure element

calc_neck_data :: forall e. HTMLElement -> EffM e NeckData
calc_neck_data element = do
  width  <- offsetWidth  element
  pure $ 
    { x_offset: 20.0
    , y_offset: 20.0
    , width: width
    , height: width * 0.15
    , num_frets: 15 }