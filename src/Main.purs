module Main where

import Data.Maybe
import Data.Tuple
import Fret
import Graphics.Canvas
import Reader
import Prelude
import NeckData

import UI.GuitarNeck

import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.AVar
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console hiding (error)

import Halogen as H
import Halogen.Aff (awaitLoad)
import Halogen.Aff as HA
import Halogen.Aff.Util
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax
import Node.FS

import DOM (DOM)
import DOM.HTML.Types 
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.HTML
import DOM.HTML.Window
import DOM.HTML.Location
import DOM.HTML.HTMLElement
import Debug.Trace

default_neck_data =
  { x_offset: 0.0
  , y_offset: 20.0
  , width: 600.0
  , height: 120.0
  , num_frets: 15 }

type EffM e a = Eff
  ( ajax :: AJAX
  , avar :: AVAR
  , console :: CONSOLE
  , canvas :: CANVAS
  , dom :: DOM
  , exception :: EXCEPTION
  , fs :: FS
  , ref :: REF | e) a

main :: forall e. EffM e Unit
main = void $ HA.runHalogenAff do
  element <- await_guitar
  neck_data <- liftEff $ calc_neck_data element
  io <- runUI (guitar_neck neck_data) unit element
  io.query (PaintNeck unit)

await_guitar :: forall e. Aff (dom :: DOM | e) HTMLElement
await_guitar = do
  awaitLoad
  element <- selectElement (QuerySelector "#content #guitar")
  maybe (throwError (error "Could not find element")) pure element

calc_neck_data :: forall e. HTMLElement -> Eff (dom :: DOM | e) NeckData
calc_neck_data element = do
  width  <- offsetWidth  element
  height <- offsetHeight element
  let _ = trace (show height) id
  pure $ 
    { x_offset: 0.0
    , y_offset: 20.0
    , width: width
    , height: height
    , num_frets: 15 }