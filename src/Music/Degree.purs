module Music.Degree where

import Prelude
import Data.Enum
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Data.Generic.Rep.Enum

data Degree = I | II | III | IV | V | VI | VII
derive instance generic_mode :: Generic Degree _
derive instance eq_degree    :: Eq Degree
derive instance ord_degree   :: Ord Degree
instance show_degree         :: Show Degree where
  show = genericShow
instance enum_degree         :: Enum Degree where
  succ = genericSucc
  pred = genericPred
instance bounded_degree      :: Bounded Degree where
  bottom = I
  top    = VII
instance boundedenum_degree  :: BoundedEnum Degree where
  cardinality = Cardinality 7
  toEnum      = defaultToEnum
  fromEnum = defaultFromEnum