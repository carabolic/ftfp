module TestQueue where

import Criterion.Main

import qualified ListQueue as L
import qualified SimpleQueue as S
import qualified SimpleQueueMod as SM
import qualified SchedQueue as SQ
import qualified Deque as D

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

testD n = D.isEmpty (drop d)
	where
		d = foldl D.addEnd D.empty [1..n]
		drop d = if D.isEmpty d then d else drop (D.tail d)

main = defaultMain [
	bgroup "ListQueue" [
		bench "1000" $ nf testL 1000,
		bench "2000" $ nf testL 2000,
		bench "3000" $ nf testL 3000,
		bench "4000" $ nf testL 4000,
		bench "5000" $ nf testL 5000,
		bench "6000" $ nf testL 6000,
		bench "7000" $ nf testL 7000,
		bench "8000" $ nf testL 8000,
		bench "9000" $ nf testL 9000,
		bench "10000" $ nf testL 10000
		],
	bgroup "SimpleQueue" [
		bench "1000" $ nf testS 1000,
		bench "2000" $ nf testS 2000,
		bench "3000" $ nf testS 3000,
		bench "4000" $ nf testS 4000,
		bench "5000" $ nf testS 5000,
		bench "6000" $ nf testS 6000,
		bench "7000" $ nf testS 7000,
		bench "8000" $ nf testS 8000,
		bench "9000" $ nf testS 9000,
		bench "10000" $ nf testS 10000
		],
	bgroup "SimpleQueueModified" [
		bench "1000" $ nf testSM 1000,
		bench "2000" $ nf testSM 2000,
		bench "3000" $ nf testSM 3000,
		bench "4000" $ nf testSM 4000,
		bench "5000" $ nf testSM 5000,
		bench "6000" $ nf testSM 6000,
		bench "7000" $ nf testSM 7000,
		bench "8000" $ nf testSM 8000,
		bench "9000" $ nf testSM 9000,
		bench "10000" $ nf testSM 10000
		],
	bgroup "SchedQueue" [
		bench "1000" $ nf testSQ 1000,
		bench "2000" $ nf testSQ 2000,
		bench "3000" $ nf testSQ 3000,
		bench "4000" $ nf testSQ 4000,
		bench "5000" $ nf testSQ 5000,
		bench "6000" $ nf testSQ 6000,
		bench "7000" $ nf testSQ 7000,
		bench "8000" $ nf testSQ 8000,
		bench "9000" $ nf testSQ 9000,
		bench "10000" $ nf testSQ 10000
		],
	bgroup "Deque" [
		bench "1000" $ nf testD 1000,
		bench "2000" $ nf testD 2000,
		bench "3000" $ nf testD 3000,
		bench "4000" $ nf testD 4000,
		bench "5000" $ nf testD 5000,
		bench "6000" $ nf testD 6000,
		bench "7000" $ nf testD 7000,
		bench "8000" $ nf testD 8000,
		bench "9000" $ nf testD 9000,
		bench "10000" $ nf testD 10000
		]
	]