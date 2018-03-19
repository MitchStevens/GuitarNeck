module Music.Transpose where

class Transpose t where
  trans :: Int -> t -> t