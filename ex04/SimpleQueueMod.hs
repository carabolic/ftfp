module SimpleQueueMod (
	Queue,
	isEmpty,
	empty,
	add,
	head,
	tail) where
	
import Prelude hiding (head, tail)

data Queue a = Queue{left::[a], lenLeft::Int, right::[a], lenRight::Int}
	deriving (Show)

empty :: Queue a
empty = Queue [] 0 [] 0

isEmpty :: Queue a -> Bool
isEmpty Queue{left=[]} = True
isEmpty _ = False

add :: Queue a -> a -> Queue a
add q@Queue{right=xs, lenRight=lr} x = norm q{right=x:xs, lenRight=lr+1}

head :: Queue a -> a
head Queue{left=[]} = error "head on empty queue"
head Queue{left=x:_} = x

tail :: Queue a -> Queue a
tail Queue{left=[]} = error "tail on empty queue"
tail q@Queue{left=_:xs, lenLeft=ll} = norm q{left = xs, lenLeft=ll-1}

norm :: Queue a -> Queue a
norm q@Queue{left=xs, lenLeft=ll, right=ys, lenRight=lr} = if length ys > length xs
	then Queue (xs ++ reverse ys) (ll + lr) [] 0
	else q