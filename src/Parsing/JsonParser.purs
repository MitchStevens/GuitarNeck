module JsonParser where

import Prelude
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.StrMap
import Fingering
import Parser
import Data.Traversable
import Text.Parsing.Parser

decode_fingering :: Json -> Either String Fingering
decode_fingering json = do
  str :: String <- decodeJson json
  fingering <- lmap show $ runParser str parse_fingering
  pure fingering

decode_fingering_map :: Json -> Either String (StrMap (Array Fingering))
decode_fingering_map json =
  let
    decode_array :: Json -> Either String (Array Fingering)
    decode_array = foldJsonArray (Left "error") (traverse decode_fingering)
  in
    foldJsonObject
      (Left $ "Couldn't decode "<> show json) 
      (traverse decode_array)
      json
