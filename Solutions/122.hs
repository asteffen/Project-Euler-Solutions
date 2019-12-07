{-
found answer here:
http://www.research.att.com/~njas/sequences/A003313
-}

import Data.List (nub, findIndex, sort)
import Data.Maybe (fromJust)
-- import Time

-- All pairs of elements in the list.
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xx@(x:xs) = map ((,) x) xx ++ pairs xs

-- All sums possible by adding a pair of elements in the list.
allSums :: [Int] -> [Int]
allSums = map (uncurry (+)) . pairs

-- All new sets (expressed as lists) possible from inserting a new element
-- that is the sum of 2 old elements.
allSets :: [Int] -> [[Int]]
allSets xs = map (: xs) $ filter (`notElem` xs) $ nub $ allSums xs

-- allPaths !! n = all sets possible after doing (n) additions.
allPaths :: [[[Int]]]
allPaths = iterate (concatMap allSets) [[1]]

-- allNums !! n = all numbers possible after doing (n) additions.
allNums :: [[Int]]
allNums = map (nub . concat) allPaths

m :: Int -> Int
m k = fromJust $ findIndex (k `elem`) allNums

--main = timePrint $ length $ allPaths2 !! 9
main = print $ sum $ map m [1..200]
-----
data Tree a = Tree a [Tree a]
	deriving (Show)

-- same as allPaths, but removes duplicate sets.
allPaths2 :: [[[Int]]]
allPaths2 = iterate (nub . concatMap (map sort . allSets)) [[1]]