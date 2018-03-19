module Reader where

import ChordFingering
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Class
import Data.Array
import Data.Either
import Data.Maybe
import Data.StrMap
import Debug.Trace
import Fret
import Music.Chord
import NeckData
import Node.FS
import Node.FS.Sync
import Prelude

import DOM.HTML.HTMLFormElement (reset)
import Data.Argonaut.Core (Json, JArray, JObject, JString, foldJsonObject, foldJsonArray, foldJsonString)
import Data.Argonaut.Parser (jsonParser)
import Data.Int (fromString)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, get)
import Node.Encoding (Encoding(..))

type F m a = forall e. m (ajax :: AJAX, ref :: REF, exception :: EXCEPTION | e) a
type EffM a = F Eff a
type AffM a = F Aff a
type FingeringMap = StrMap (Array ChordFingering)

moveable_fingerings :: EffM (Ref FingeringMap)
moveable_fingerings = newRef empty <* init
  where
    init = launchAff $ do
      res :: AffjaxResponse String <- get "http://www.mitchstevens.com/guitar/barre_chords"
      let _ = trace (show res.response) id
      pure unit

{-
    init = launchAff_ $ do
      barre_chords <- get "http://www.mitchstevens.com/guitar/barre_chords"
      let a = barre_chords.response :: String
      let _ = trace (show barre_chords.response) id
  --file <- readTextFile UTF8 "barre_chords.json"
  --fingerings <- case jsonParser file of
  --    Left  str  -> throwException $ error str
  --    Right json -> pure $ read_fingerings json
      pure unit
-}




get_fingerings :: forall e. NeckData -> Chord -> EffM (Array FingeringData)
get_fingerings neck chord = do
  fingerings <- all_fingerings chord
  if null fingerings
    then throwException $ error ("Couldn't find any chord fingerings for chord " <> show chord)
    else pure $ mapMaybe (cache_centeroid neck) fingerings

all_fingerings :: forall e. Chord -> EffM (Array ChordFingering)
all_fingerings chords = do
  table <- moveable_fingerings >>= readRef
  pure $ fromMaybe [] (lookup "min7" table)

read_fingerings :: Json -> StrMap (Array ChordFingering)
read_fingerings = foldJsonObject empty (map (foldJsonArray [] read_block))
  where 
  
  read_block :: JArray -> Array ChordFingering
  read_block jarray = mapMaybe (foldJsonString Nothing read_fingering) jarray

  -- E-A-D-G-B-e
  read_fingering :: JString -> Maybe ChordFingering
  read_fingering str = 
    let frets = (fromString >=> as_fret) <$> split (Pattern "-") str
    in case frets of
      [e2, a2, d3, g3, b3, e4] -> Just $ Fingering {e4: e4, b3: b3, g3: g3, d3: d3, a2: a2, e2: e2}
      _ -> Nothing
