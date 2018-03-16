module Fret where

import Prelude
import Data.Maybe

data Fret = Open | Fret Int

fret_num :: Fret -> Int
fret_num = case _ of
  Open   -> 0
  Fret n -> n