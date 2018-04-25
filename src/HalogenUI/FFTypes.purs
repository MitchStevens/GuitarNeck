module UI.FFTypes (EffM, AffM) where
  
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)

import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Graphics.Canvas (CANVAS)
import Network.HTTP.Affjax (AJAX)
import Node.FS (FS)

type EffM e = Eff
  ( ajax :: AJAX
  , avar :: AVAR
  , console :: CONSOLE
  , canvas :: CANVAS
  , dom :: DOM
  , exception :: EXCEPTION
  , fs :: FS
  , ref :: REF | e)

type AffM e = Aff
  ( ajax :: AJAX
  , avar :: AVAR
  , canvas :: CANVAS
  , console :: CONSOLE
  , dom :: DOM
  , fs :: FS
  , exception :: EXCEPTION
  , ref :: REF | e)