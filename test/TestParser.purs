module TestParser where

import Prelude
import Data.Maybe
import Fingering
import Fret

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.AVar

import Test.Unit 
import Test.Unit.Main
import Test.Unit.Console

import Data.Array
import Data.Argonaut.Core
import Data.Argonaut.Core as JSON
import Data.Argonaut.Parser (jsonParser)
import Data.Either
import Data.Foldable (traverse_)
import Data.Int (fromString)
import Data.String hiding (length)
import Data.StrMap

import Text.Parsing.Parser
import JsonParser
import Parser

test_chords :: Array String
test_chords = ["x-3-5-5-5-3", "x-x-5-5-5-8", "8-10-10-9-8-8", "x-x-10-12-x-12", "x-3-5-5-4-3", "8-10-10-8-8-8", "x-x-13-12-13-11", "x-x-1-2-1-2", "x-x-4-5-4-5", "x-x-7-8-7-8", "x-x-10-11-10-11", "x-3-2-1-1-x", "x-x-10-9-9-8", "x-3-5-4-5-x", "8-x-9-9-8-x", "x-x-5-5-5-7", "x-x-10-9-8-7", "x-x-10-12-12-12", "x-3-1-3-4-x", "x-3-5-3-4-3", "x-x-5-5-4-6", "8-x-8-8-8-x", "x-10-8-8-11-x", "x-3-2-3-1-x", "x-x-5-5-5-6", "8-10-8-9-8-8", "x-x-10-12-11-12", "x-3-4-3-4-x", "x-x-4-5-4-6", "8-x-8-8-7-x", "x-x-10-11-11-11"]

test_strmap1 :: Either String Json
test_strmap1 = jsonParser """{
  "maj": ["x-3-5-5-5-3", "x-x-5-5-5-8", "8-10-10-9-8-8", "x-x-10-12-x-12"],
  "min": ["x-3-5-5-4-3", "8-10-10-8-8-8", "x-x-13-12-13-11"],
  "dim": ["x-x-1-2-1-2", "x-x-4-5-4-5", "x-x-7-8-7-8", "x-x-10-11-10-11"],
	"aug": ["x-3-2-1-1-x", "x-x-10-9-9-8"],
	"maj7": ["x-3-5-4-5-x", "8-x-9-9-8-x", "x-x-5-5-5-7", "x-x-10-9-8-7", "x-x-10-12-12-12"],
	"min7": ["x-3-1-3-4-x", "x-3-5-3-4-3", "x-x-5-5-4-6", "8-x-8-8-8-x", "x-10-8-8-11-x"],
	"7": ["x-3-2-3-1-x", "x-x-5-5-5-6", "8-10-8-9-8-8", "x-x-10-12-11-12"],
	"min7(b5)": ["x-3-4-3-4-x", "x-x-4-5-4-6", "8-x-8-8-7-x", "x-x-10-11-11-11"]
}
"""

run_tests :: forall e. Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR| e ) Unit
run_tests = runTest do
  suite "parse json" do
    test "chord_fingerings" $ traverse_ parse_chord_fingering_test test_chords
    test "strmap fingerings" parse_strmap_test

parse_chord_fingering_test :: forall e. String -> Test e
parse_chord_fingering_test str = case runParser str parse_fingering of
  Left  _ -> failure $ "didn't parse "<> str <>" properly!"
  Right _ -> success

parse_strmap_test :: forall e. Test e
parse_strmap_test = case test_strmap1 >>= decode_fingering_map of
  Left  err   -> failure err
  Right fings -> if size fings == 8 then success
    else failure $ "didn't parse "<> show test_strmap1 <>" properly!"
