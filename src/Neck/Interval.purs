module Interval (
  Interval (..),
  interval,
  is_empty,
  is_degenerate,
  in_interval,
  within,
  singleton
) where

import Data.Foldable
import Data.Maybe
import Prelude

import Control.Monad.Eff.AVar (AVarStatus(..))
import Data.Lattice as L

data Interval a = Interval { min :: a, max :: a } | EmptyInterval
instance eq_interval :: Eq a => Eq (Interval a) where
  eq (Interval a) (Interval b) = a.min == b.min && a.max == b.max
  eq EmptyInterval EmptyInterval = true
  eq _ _ = false
instance num_interval :: (Semiring a, Ord a) => Semiring (Interval a) where
  zero = Interval { min: zero, max: zero }
  one  = Interval { min: one,  max: one }
  add (Interval a) (Interval b) = Interval {min: a.min+b.min, max: a.max+b.max}
  add _ _ = EmptyInterval
  mul (Interval {min:x1, max:y1}) (Interval {min:x2, max:y2}) = Interval
    { min: fromMaybe zero (minimum l)
    , max: fromMaybe zero (maximum l) }
    where l = [x1*y1, x1*y2, x2*y1, x2*y2]
  mul _ _ = EmptyInterval
instance ring_interval :: (Ring a, Ord a) => Ring (Interval a) where
  sub (Interval a) (Interval b) = Interval {min: a.min-b.min, max: a.max-b.max}
  sub _ _ = EmptyInterval
instance show_interval :: (Show a) => Show (Interval a) where
  show EmptyInterval = "{}"
  show (Interval r) = "["<> show (r.min) <>", "<> show (r.max) <>"]"

singleton :: forall a. Ord a => a -> Interval a
singleton x = interval x x

--Lattice instances
instance join_interval :: Ord a => L.JoinSemilattice (Interval a) where
  join (Interval a) (Interval b) =
    interval (min a.min b.min) (max a.max b.max)
  join a EmptyInterval = a
  join EmptyInterval b = b
instance meet_interval :: Ord a => L.MeetSemilattice  (Interval a) where
  meet (Interval a) (Interval b) =
    interval (max a.min b.min) (min a.max b.max)
  meet a EmptyInterval = EmptyInterval
  meet EmptyInterval b = EmptyInterval
instance boundedjoin_interval :: Ord a => L.BoundedJoinSemilattice (Interval a) where
  bottom = EmptyInterval
instance boundedmeet_interval :: Bounded a => L.BoundedMeetSemilattice (Interval a) where
  top = interval bottom top
instance latticejoin_interval :: Bounded a => L.Lattice (Interval a)


interval :: forall a. Ord a => a -> a -> Interval a
interval min max = if min > max then EmptyInterval
  else Interval { min: min, max: max }

is_empty :: forall a. Ord a => Interval a -> Boolean
is_empty EmptyInterval = true
is_empty _ = false

is_degenerate :: forall a. Ord a => Interval a -> Boolean
is_degenerate (Interval a) = a.min == a.max
is_degenerate EmptyInterval = false

in_interval :: forall a. Ord a => a -> Interval a -> Boolean
in_interval x (Interval a) = a.min <= x && x <= a.max
in_interval x EmptyInterval = false

{- Is x contained entirely with y? -}
within :: forall a. Ord a => Interval a -> Interval a -> Boolean
within (Interval x) (Interval y) = y.min <= x.min && x.max <= y.max
within x EmptyInterval = false
within EmptyInterval _ = true