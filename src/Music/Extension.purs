module Music.Extension where

import Prelude

data Extension = Sharp Int | Flat Int | Add Int | Sus2 | Sus4
derive instance eq_extension :: Eq Extension

instance show_extension :: Show Extension where
  show = case _ of 
    Sharp n -> "#" <> show n
    Flat n  -> "b" <> show n
    Add n   -> show n
    Sus2    -> "sus2"
    Sus4    -> "sus4"

degree :: Extension -> Int
degree = case _ of 
  Sharp n -> n
  Flat n  -> n
  Add n   -> n
  Sus2    -> 2
  Sus4    -> 4