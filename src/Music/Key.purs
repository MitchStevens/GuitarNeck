module Music.Key (
  Key,
  Keyed (..)
) where

import Prelude
import Music.Mode
import Music.Note
import Data.Foldable
import Data.Traversable
import Data.Array
import Data.Enum
import Control.Comonad
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Data.Generic.Rep.Enum

type Key = { pitch :: PitchClass, mode :: Mode }

data Keyed a = Keyed Key a
derive instance eq_keyed      :: Eq a => Eq (Keyed a)
derive instance functor_keyed :: Functor Keyed
instance extend_keyed         :: Extend Keyed where
  extend f keyed@(Keyed k x) = Keyed k (f keyed)
instance comonad_keyed        :: Comonad Keyed where
  extract (Keyed k x) = x
instance foldable_keyed       :: Foldable Keyed where
  foldr f z (Keyed k x) = f x z
  foldl f z (Keyed k x) = f z x
  foldMap f (Keyed k x) = f x
instance traversable_keyed    :: Traversable Keyed where
  traverse f xs = sequence (f <$> xs)
  sequence (Keyed k trav) = map (Keyed k) trav
instance show_keyed           :: (Show s) => Show (Keyed s) where
  show (Keyed key s) =
    fold ["(", show s, ": ", show key.pitch, show key.mode, ")"]