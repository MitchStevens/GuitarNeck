module Music.Transpose (
  class Transpose,
  trans,
  mod12
) where

import Prelude

mod12 :: Int -> Int
mod12 x = mod (mod x 12 + 12) 12

class Transpose t where
  trans :: Int -> t -> t

instance transpose_functor :: (Transpose t, Functor f) => Transpose (f t) where
  trans = map <<< trans