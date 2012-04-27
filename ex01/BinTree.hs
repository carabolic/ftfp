module BinTree where

data BinTree a = Node (BinTree a) a (BinTree a) | Nil
--	deriving (Show)

-- Assuming compare function returns true iff the argument is smaller than the second
insert :: (a -> a -> Bool) -> a -> BinTree a -> BinTree a
insert comp elem Nil = Node Nil elem Nil
insert comp elem (Node left cur right) = if comp elem cur
					    then Node (insert comp elem left) cur right
					    else Node left cur (insert comp elem right)

insertList :: (a -> a -> Bool) -> [a] -> BinTree a -> BinTree a
insertList comp list tree = foldl (\t e -> insert comp e t) tree list

greatest :: BinTree a -> Maybe a
greatest Nil = Nothing
greatest (Node Nil elem Nil) = Just elem
greatest (Node _ _ right) = greatest right

inorder :: BinTree a -> [a]
inorder Nil = []
inorder (Node Nil elem Nil) = [elem]
inorder (Node left elem right) = inorder left ++ [elem] ++ inorder right

height :: BinTree a -> Int
height Nil = 0
height (Node left _ right) = 1 +  max (height left) (height right)

toString :: Show a => BinTree a -> String
toString (Node Nil val Nil) = show val
toString (Node left val right) =

instance Show a => Show (BinTree a) where
	show = toString
