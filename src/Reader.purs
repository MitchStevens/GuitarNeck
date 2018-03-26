module Reader where

import Prelude
import Parsing
import Fingering
import Fret
import NeckData
import Music

import Control.Apply
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Aff.AVar
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Class

import Debug.Trace
import JsonParser

import Data.Array
import Data.Argonaut.Core (Json, JArray, JObject, JString, foldJsonObject, foldJsonArray, foldJsonString)
import Data.Argonaut.Parser (jsonParser)
import Data.Either
import Data.Int (fromString)
import Data.Maybe
import Data.String (Pattern(..), split)
import Data.StrMap
import Data.Traversable (traverse)

import DOM.HTML.HTMLFormElement (reset)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, get)

type AffM e a = Aff
  ( ajax :: AJAX
  , avar :: AVAR
  , ref :: REF
  , exception :: EXCEPTION | e) a

type FingeringCache =
  { open     :: StrMap (Array Fingering)
  , moveable :: StrMap (Array Fingering)
  }

read_fingerings :: forall e. AffM e FingeringCache
read_fingerings = do
  let decode = decode_fingering_map <<< (\x -> x.response)
  let cache open move = {open: open, moveable: move}
  open_eith <- decode <$> get "http://localhost:9000/chords/open"
  move_eith <- decode <$> get "http://localhost:9000/chords/moveable"
  case lift2 cache open_eith move_eith of
    Left err    -> throwError $ error err
    Right cache -> pure cache

get_fingerings :: forall e. FingeringCache -> Chord -> Array Fingering
get_fingerings fingerings chord@(Chord root _ _) =
  let suffix = show_quality chord <> show_extensions chord
      note_num = pcToInt root
      a = trace (show suffix) id
  in map (trans note_num) $ fromMaybe [] $ lookup suffix fingerings.moveable