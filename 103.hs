{-
compiled -O:

running time = 2341 sec
output = [20,31,38,39,40,42,45]
-}

import Data.List (nub)
import Time

secondRule :: [Integer] -> Bool
secondRule xs = all qualifies  [2 .. (length xs + 1) `div` 2]
	where qualifies i = sum (take i xs) > sum (take (i - 1) $ reverse xs)

{-
If the second rule is satisfied, then any two subsets with different amounts of
elements must have different sums.

Thus, for the first rule it is only necessary to test equality for subsets
which have the same amount of elements.
-}

-- All ways to choose (n) elements from (xs).
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x :) (choose (n - 1) xs) ++ choose n xs

allElemsUnique :: (Eq a) => [a] -> Bool
allElemsUnique xs = xs == nub xs

-- Returns (True) if all sums of (n)-element subsets of (xs) are distinct.
distinctSums :: [Integer] -> Int -> Bool
distinctSums xs n = allElemsUnique $ map sum $ choose n xs

firstRule :: [Integer] -> Bool
firstRule xs = all (distinctSums xs) [2 .. length xs `div` 2]

isSpecialSumSet :: [Integer] -> Bool
isSpecialSumSet xs = secondRule xs && firstRule xs

-- Input: number of elements, sum of elements, minimum element
-- Output: all lists that satisfy the input.
allSets :: Int -> Integer -> Integer -> [[Integer]]
allSets 1 s _ = [[s]]
allSets n s m = concatMap (\x -> map (x :) $ allSets (n - 1) (s - x) (x + 1)) [m .. (s - 1) `div` 2]

-- The optimum special sum set of a given size.
optimumSSS :: Int -> [Integer]
optimumSSS n = head $ filter isSpecialSumSet $ concatMap (\s -> allSets n s 1) [1..]

main :: IO ()
main = timePrint $ optimumSSS 7