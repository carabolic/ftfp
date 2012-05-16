module Deque (
	-- * Auxillary Functions
	Deque,
	-- ** Checks if the given Deque is empty
	isEmpty,
	-- ** Constructor
	empty,
	-- * Functions on the front element
	-- ** Add an element to the front of the Deque
	addFront,
	-- ** Add an element to the front of the Deque
	head,
	-- ** Cut of the front element of the Deque
	tail,
	-- * Functions on the rear element
	-- ** Add an element to the end of the Deque
	addEnd,
	-- ** Get the element from the end of the Deque
	last,
	-- ** Cut of the end element of the Deque
	init) where
	
import Prelude hiding (head, tail, last, init)

c = 2

data Deque a =
	Deque{left::[a], lenLeft::Int, right::[a], lenRight::Int}
	deriving (Show)

empty :: Deque a
empty = Deque [] 0 [] 0

isEmpty :: Deque a -> Bool
isEmpty Deque{lenLeft=ll, lenRight=lr} = (ll + lr) == 0

-- ** Checks if lenght left <= c * lenght right + 1 AND lenght right <= c * length left + 1
norm :: Deque a -> Deque a
norm d@Deque{left=l, lenLeft=ll, right=r, lenRight=lr}
	-- Left list is too long
	| ll > c * lr + 1 =
		let
			l' = take i l
			r' = r ++ (reverse $ drop i l)
		in d{left=l', lenLeft=i, right=r', lenRight=j}
	-- Right list is too long
	| lr > c * ll + 1 =
		let
			l' = l ++ (reverse $ drop j r)
			r' = take j r
		in d{left=l', lenLeft=i, right=r', lenRight=j}
	| otherwise = d
	where
		i = (ll + lr) `div` 2
		j = ll + lr - i

addFront :: Deque a -> a -> Deque a
addFront d@Deque{left=xs, lenLeft=ll} x = norm d{left=x:xs, lenLeft=ll+1}

head :: Deque a -> a
head Deque{left=[], right=[]} = error "Head on empty Deque"
head Deque{left=[], right=x:_} = x
head Deque{left=x:_} = x

tail :: Deque a -> Deque a
tail Deque{left=[], right=[]} = error "Tail on empty Deque"
tail d@Deque{left=[], right=_:xs} = empty
tail d@Deque{left=_:xs, lenLeft=ll} = norm d{left=xs, lenLeft=ll-1}

addEnd :: Deque a -> a -> Deque a
addEnd d@Deque{right=xs, lenRight=lr} x = norm d{right=x:xs, lenRight=lr+1}

last :: Deque a -> a
last Deque{left=[], right=[]} = error "Last on empty Deque"
last Deque{left=x:_, right=[]} = x
last Deque{right=x:_} = x

init :: Deque a -> Deque a
init Deque{left=[], right=[]} = error "Init on empty Deque"
init Deque{left=x:_, right=[]} = empty
init d@Deque{right=_:xs, lenRight=lr} = norm d{right=xs, lenRight=lr-1}