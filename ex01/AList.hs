module AList where

type AList a = [(Char, a)]

nil :: AList a
nil = []

del :: AList a -> Char -> AList a
del alist c = filter ((/= c) . fst) alist

put :: AList a -> Char -> a -> AList a
put alist c x = (c, x) : (del alist c)

get :: AList a -> Char -> Maybe a
get alist c = case filter ((== c) . fst) alist of
		   [] -> Nothing
		   (_, x):ps -> Just x

isIn :: AList a -> Char -> Bool
isIn alist c = not $ null $ filter ((== c) . fst) alist

isNil :: AList a -> Bool
isNil alist = null alist