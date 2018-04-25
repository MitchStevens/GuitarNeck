module LimitQueue where

import Partial
import Prelude

import Data.Maybe
import Data.Tuple
import Data.Unfoldable
import Data.Sequence as S

newtype LimitQueue a = LimitQueue { limit :: Int, seq :: S.Seq a}

empty :: forall a. Partial => Int -> LimitQueue a
empty n = if n < 0 then crashWith "Can't have a negative sized LimitQueue"
  else LimitQueue { limit: n, seq: S.empty }

insert :: forall a. a -> LimitQueue a -> LimitQueue a
insert x (LimitQueue q) =
  if S.length q.seq < q.limit
    then LimitQueue $ q { seq = S.cons x q.seq }
    else LimitQueue $ q { seq = maybe S.empty fst (S.unsnoc (S.cons x q.seq)) }

toArray :: forall a. LimitQueue a -> Array a
toArray (LimitQueue q) = S.toUnfoldable q.seq