module Parser (
  parse_chord,
  parse_fingering
) where

import Control.Plus
import Data.Functor
import Data.Identity
import Music
import Prelude
import Fingering
import Fret

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String (char, satisfy, string, eof)
import Text.Parsing.Parser.Token

import Data.Array (some, many, fromFoldable)
import Data.Either
import Data.Foldable (oneOf)
import Data.String (toCharArray, fromCharArray)
import Data.Maybe
import Data.Int (fromString)
import Data.Char.Unicode (isAlpha)
import Data.List

type ParserS a = ParserT String Identity a

parse_note :: ParserS PitchClass
parse_note = do
  note <- note_num
  accidental <- option 0 parse_accidental
  pure $ pitch (note + accidental)
  where
    note_num :: ParserS Int
    note_num = oneOf
      [ char 'C' *> pure 0
      , char 'D' *> pure 2
      , char 'E' *> pure 4
      , char 'F' *> pure 5
      , char 'G' *> pure 7
      , char 'A' *> pure 9
      , char 'B' *> pure 11 ]

parse_accidental :: ParserS Int
parse_accidental = oneOf
  [ char '#' *> pure 1
  , char 'b' *> pure (-1)
  , char '♮' *> pure 0 ]

parse_mode :: ParserS Mode
parse_mode = option Mixolydian $ choice [majP, minP, domP, augP, dimP]
  where
    majP = (try (string "Maj")
        <|> try (string "maj")
        <|> try (string "M"   <* end)
        <|> try (string "Δ")) *> pure Major
    minP = (try (string "m"   <* end)
        <|> try (string "min")
        <|> try (string "Min")
        <|> try (string "-"   <* end)) *> pure Minor
    domP =  try (string "dom" <* end)  *> pure Mixolydian
    augP = (string "+" <|> string "aug") *> pure Augmented
    dimP = (string "o" <|> string "dim") *> pure Diminished
    end = lookAhead (try (void $ satisfy (not <<< isAlpha)) <|> eof)

parse_head_ext :: ParserS (Array Extension)
parse_head_ext = oneOf
  [ parse_extension <#> pure
  , string "sus2" *> pure [Sus2]
  , string "sus4" *> pure [Sus4]
  ]

parse_tail_ext :: ParserS Extension
parse_tail_ext = char '(' *> parse_extension <* char ')'

parse_extension :: ParserS Extension
parse_extension = oneOf
  [ char 'b' *> parse_ext_num <#> Flat
  , char '#' *> parse_ext_num <#> Sharp
  , parse_ext_num <#> Add]
  where
    parse_ext_num :: ParserS Int
    parse_ext_num = oneOf
      [ char '5'    *> pure 5
      , char '6'    *> pure 6
      , char '7'    *> pure 7
      , char '9'    *> pure 9
      , string "11" *> pure 11
      , string "13" *> pure 13 ]

parse_chord :: ParserS Chord
parse_chord = do
  root <- parse_note
  mode <- parse_mode
  head_ext <- option [] parse_head_ext
  tail_exts <- many parse_tail_ext
  eof
  case mk_chord root mode (head_ext <> tail_exts) of
    Left err    -> fail (show err)
    Right chord -> pure chord

parse_int :: ParserS Int
parse_int = do
  num_str <- fromCharArray <$> some digit
  case fromString num_str of
    Just n  -> pure n
    Nothing -> fail $ "Couldn't parse "<> num_str <>"."

parse_fret :: ParserS (Maybe Fret)
parse_fret = (map (Just<<<Fret) parse_int) <|> (char 'x' *> pure Nothing)

parse_fingering :: ParserS Fingering
parse_fingering = do
  nums <- map fromFoldable $ sepBy parse_fret (char '-')
  case nums of
    [e2, a2, d3, g3, b3, e4] -> pure $ Fingering {e4: e4, b3: b3, g3: g3, d3: d3, a2: a2, e2: e2}
    _                        -> fail $ "Couldn't parse "<> show nums <>"."