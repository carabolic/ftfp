module TestQueue where

import qualified ListQueue as L
import qualified SimpleQueue as S
import qualified SimpleQueueMod as SM
import qualified SchedQueue as SQ

testL n = L.isEmpty (drop q)
  where
    q = foldl L.add L.empty [1..n]
    drop q = if L.isEmpty q then q else drop (L.tail q)

testS n = S.isEmpty (drop q)
  where
    q = foldl S.add S.empty [1..n]
    drop q = if S.isEmpty q then q else drop (S.tail q)

testSM n = SM.isEmpty (drop q)
  where
    q = foldl SM.add SM.empty [1..n]
    drop q = if SM.isEmpty q then q else drop (SM.tail q)

testSQ n = SQ.isEmpty (drop q)
	where
		q = foldl SQ.add SQ.empty [1..n]
		drop q = if SQ.isEmpty q then q else drop (SQ.tail q)