module ListQueue (
	Queue,
	empty,
	isEmpty,
	add,
	head,
	tail) where

import Prelude hiding (head, tail)

data Queue a = Queue [a]

empty :: Queue a
empty = Queue []

isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty _ = False

add :: Queue a -> a -> Queue a
add (Queue []) val = Queue [val]
add (Queue list) val = Queue (list ++ [val])

head :: Queue a -> a
head (Queue (x:_)) = x

tail :: Queue a -> Queue a
tail (Queue (_:xs)) = Queue xs