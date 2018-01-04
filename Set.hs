module Set where

type Set a = [a]

subset :: (Eq a) => Set a -> Set a -> Bool
subset [] __ = True
subset __ [] = False
subset (x:xs) y = (x `elem` y) && subset xs y

intersect :: (Eq a) => Set a -> Set a -> Set a
intersect xs ys = foldl  (\acc x -> if x `elem` ys then x : acc else acc) [] xs

union :: (Eq a) => Set a -> Set a -> Set a
union xs ys = foldl (\acc x -> if x `elem` ys then acc else x : acc) ys xs

lessThanOrEqualTo :: (Eq a, Ord a) => a -> Set a -> Bool
lessThanOrEqualTo x ys = and [x <= y | y <- ys]

lessThanOrEqualToSet :: (Eq a, Ord a) => Set a -> a -> Bool
lessThanOrEqualToSet xs y = and [y <= x | x <- xs]
