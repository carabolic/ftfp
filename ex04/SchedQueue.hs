module SchedQueue (
	Queue,
	isEmpty,
	empty,
	add,
	head,
	tail) where
	
import Prelude hiding (head, tail)

data Queue a = Queue{left::[a], sched::[a], right::[a]}
	deriving (Show)

empty :: Queue a
empty = Queue [] [] []

isEmpty :: Queue a -> Bool
isEmpty Queue{left=[]} = True
isEmpty _ = False

add :: Queue a -> a -> Queue a
add q@Queue{right=xs} x = norm q{right=x:xs}

head :: Queue a -> a
head Queue{left=[]} = error "head on empty queue"
head Queue{left=x:_} = x

tail :: Queue a -> Queue a
tail Queue{left=[]} = error "tail on empty queue"
tail q@Queue{left=_:xs} = norm q{left = xs}

norm :: Queue a -> Queue a
norm q@Queue{sched=[]} = Queue l l []
	where l = rotate q
norm q@Queue{left=xs, sched=s:ss, right=ys} = q{sched=ss}
			
rotate :: Queue a -> [a]
rotate Queue{left=[], sched=ss, right=y:_} = y:ss
rotate q@Queue{left=x:xs, sched=ss, right=y:ys} =
	x : rotate q{left=xs, sched=y:ss, right=ys}