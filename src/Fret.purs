module Fret where

import Prelude ((&&), (<), (<=))
import Data.Maybe (Maybe(..))

data Fret = Open | Fret Int

fret_num :: Fret -> Int
fret_num = case _ of
  Open   -> 0
  Fret n -> n

as_fret :: Int -> Maybe Fret
as_fret 0 = Just Open
as_fret n = if 0 < n && n <= 24 then Just (Fret n) else Nothing