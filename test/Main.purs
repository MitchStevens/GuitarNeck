module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Unit.Console
import TestParser as TestParser
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.AVar

main :: forall e. Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR| e ) Unit
main = TestParser.run_tests
