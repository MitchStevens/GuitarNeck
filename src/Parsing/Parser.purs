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
import Text.Parsing.Parser.String (char, satisfy, string, eof, anyChar)
import Text.Parsing.Parser.Token

import Data.Array (some, many, fromFoldable)
import Data.Either
import Data.Foldable (oneOf, fold)
import Data.String (toCharArray, fromCharArray)
import Data.Maybe
import Data.Int (fromString)
import Data.Char.Unicode (isAlpha)

type ParserS a = ParserT String Identity a

parse_note :: ParserS PitchClass
parse_note = do
  note <- choice [note_num, err_lower, err, err_empty]
  accidental <- option 0 parse_accidental
  pure $ pitch (note + accidental)
  where
    note_num :: ParserS Int
    note_num = oneOf
      [ char 'C' $> 0
      , char 'D' $> 2
      , char 'E' $> 4
      , char 'F' $> 5
      , char 'G' $> 7
      , char 'A' $> 9
      , char 'B' $> 11 ]

    err_lower = oneOf (map char $ toCharArray "abcdefg")
      *> fail "Note names must be in uppercase, eg. 'E' not 'e'"

    err_empty = eof *> fail "Waiting for chord..."

    err = fail "Couldn't read root note. Examples of valid root notes: 'A', 'Bb', 'F#'"

parse_accidental :: ParserS Int
parse_accidental = oneOf
  [ char '#' $> 1
  , char 'b' $> (-1)
  , char '♮' $> 0 ]

parse_mode :: ParserS Mode
parse_mode = option Mixolydian $ choice [majP, minP, domP, augP, dimP]
  where
    majP = (try (string "Maj")
        <|> try (string "maj")
        <|> try (string "M"   <* end)
        <|> try (string "Δ")) $> Major
    minP = (try (string "m"   <* end)
        <|> try (string "min")
        <|> try (string "Min")
        <|> try (string "-"   <* end)) $> Minor
    domP =  try (string "dom" <* end)  $> Mixolydian
    augP = (string "+" <|> string "aug") $> Augmented
    dimP = (string "o" <|> string "dim") $> Diminished
    end = lookAhead (try (void $ satisfy (not <<< isAlpha)) <|> eof)

parse_head_ext :: ParserS (Array Extension)
parse_head_ext = choice
  [ parse_extension <#> pure
  , string "sus2" $> [Sus2]
  , string "sus4" $> [Sus4]
  ]

parse_tail_ext :: ParserS Extension
parse_tail_ext = char '(' *> parse_extension <* char ')'

parse_extension :: ParserS Extension
parse_extension = choice
  [ char 'b' *> parse_ext_num <#> Flat
  , char '#' *> parse_ext_num <#> Sharp
  , parse_ext_num <#> Add
  , fail "Couldn't read extension" ]
  where
    parse_ext_num :: ParserS Int
    parse_ext_num = choice
      [ char '5'    $> 5
      , char '6'    $> 6
      , char '7'    $> 7
      , char '9'    $> 9
      , string "11" $> 11
      , string "13" $> 13
      , fail "Couldn't parse Extension" ]

parse_chord :: ParserS Chord
parse_chord = do
  root <- parse_note
  mode <- parse_mode
  head_ext <- option [] parse_head_ext
  tail_exts <- many parse_tail_ext <?> "^ext error"
  eof <?> "Couldn't understand this chord"
  pure $ Chord root mode (head_ext <> tail_exts)


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