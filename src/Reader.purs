module Reader (
  FingeringCache,
  read_fingerings,
  get_fingerings
) where

import Control.Apply
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Aff.Class
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Debug.Trace

import Data.Argonaut.Parser (jsonParser)
import Data.Array
import Data.Either
import Data.Int (fromString)
import Data.Maybe
import Data.String (Pattern(..), split)
import Data.StrMap
import Data.Argonaut.Core (Json, JArray, JObject, JString, foldJsonObject, foldJsonArray, foldJsonString)
import Data.Traversable (traverse)

import Fingering
import Fret
import JsonParser
import Music
import NeckData
import Parsing
import Prelude

import DOM.HTML.HTMLFormElement (reset)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, get)

type AffM e a = Aff
  ( ajax :: AJAX
  , avar :: AVAR
  , console :: CONSOLE
  , ref :: REF
  , exception :: EXCEPTION | e) a

type FingeringCache =
  { open     :: StrMap (Array Fingering)
  , moveable :: StrMap (Array Fingering)
  }

read_fingerings :: forall e. String -> AffM e FingeringCache
read_fingerings origin = do
  let decode = decode_fingering_map <<< (\x -> x.response)
  let cache open move = {open: open, moveable: move}
  open_eith <- decode <$> get (origin<>"/chords/open")
  move_eith <- decode <$> get (origin<>"/chords/moveable")
  case lift2 cache open_eith move_eith of
    Left err    -> throwError $ error err
    Right cache -> pure cache

get_fingerings :: FingeringCache -> Chord -> Array Fingering
get_fingerings fingerings chord@(Chord root _ _) =
  let suffix = chord_suffix chord
      note_num = trace suffix $ \_ -> pcToInt root
  in map (shift_fingering note_num) $ fromMaybe [] $ lookup suffix fingerings.moveable