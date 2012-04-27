module ListFunc where

-- Keeps the given list as is, but removes duplicates
f1 :: [Integer] -> [Integer]
f1 l = f1' [] l
	where
	f1' l [] = reverse l
	f1' l (x:xs)
		| x `isIn` l = f1' l xs
		| otherwise = f1' (x:l) xs
	isIn x [] = False
	isIn x (y:ys) = if x == y then True else isIn x ys

-- f1 transformation using only functions from prelude
f1Lib :: [Integer] -> [Integer]
f1Lib list = foldl (\l x -> if elem x l then l else l ++ [x]) [] list

-- Test all implementations of f1
f1Test list = ("Reference", f1 list) : ("Library", f1Lib list) : []


-- Builds the Horner-Schema
f2 :: Double -> [Double] -> Double
f2 x [] = 0
f2 x (a:as) = a + x * (f2 x as)

-- f2 transformation using only functions from prelude
f2Lib :: Double -> [Double] -> Double
f2Lib x list = foldl (\s t -> x * s + t) 0 (reverse list)

-- f2 transformation using tail recursion
f2Tail :: Double -> [Double] -> Double
f2Tail x list = helper x list 0 1
	where
		helper _ [] r _ = r
		helper x (a:as) r s = helper x as (r + s * a) (s * x)
		
-- Test all implementation of f2
f2Test x list = ("Reference", f2 x list) : ("Library", f2Lib x list) : 
	("Tail Recursive", f2Tail x list) : []

-- Returns the maximum value or nothing if list is empty
f3 :: [Integer] -> Maybe Integer
f3 [] = Nothing
f3 (x:xs) = Just $ f3' xs x
	where
		f3' [] a = a
		f3' (x:xs) a
			| a < x = f3' xs x
			| otherwise = f3' xs a

-- f3 transformation using only functions from prelude
f3Lib :: [Integer] -> Maybe Integer
f3Lib [] = Nothing
f3Lib (x:xs) = Just $ foldl (\a b -> if b > a then b else a) x xs

-- Test all implementations of f3
f3Test list = ("Reference", f3 list) : ("Library", f3Lib list) : []

-- Reverses the given list (linear recursion)
f4 :: [a] -> [a]
f4 [] = []
f4 (x:xs) = f4 xs ++ [x]

-- Linear recursion without pattern matching
f4' :: [a] -> [a]
f4' list = if null list
	then list
	-- The concat operator is ASSIOCIATIVE, hence could be rewritten in
	-- tail recursive manner, e.g.: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
	-- There exists a neutral element '[]' for the concat operator,
	-- e.g.: [] ++ xs = xs = xs ++ []
	else f4' (tail list) ++ [head list]

-- f4 transformation using only functions from prelude
f4Lib :: [a] -> [a]
f4Lib = reverse

-- f4 transformation using tail recursion
f4Tail :: [a] -> [a]
f4Tail list = helper list []
	where
		helper [] list = list
		helper (x:xs) list = helper xs ([x] ++ list)

-- f4 transformation using tail recursion and list constructor
f4Tail' :: [a]	-> [a]
f4Tail' (x:xs) = helper [x] xs
	where
		helper list [] = list
		helper list (x:xs) = helper (x : list) xs

f4Test list = ("Reference", f4 list) : ("Reference Rewrite", f4' list) : 
	("Library", f4Lib list) : ("Tail Recursive", f4Tail list) :
	("Tail Recursive Rewrite", f4Tail' list) : []

-- Traverses to list an applies the given function to each pair
f5 :: (a -> b -> c) -> [a] -> [b] -> [c]
f5 f [] _ = []
f5 f _ [] = []
f5 f (x:xs) (y:ys) = f x y : f5 f xs ys

-- f5 transformation using only functions from the prelude
f5Lib :: (a -> b -> c) -> [a] -> [b] -> [c]
f5Lib func l1 l2 = map (\(x, y) -> func x y) $ zip l1 l2

f5Test func l1 l2 = ("Reference", f5 func l1 l2) : ("Library", f5Lib func l1 l2) : []