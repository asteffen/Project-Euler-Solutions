{-
there are 6 ways to choose an operator:
- addition
- multiplication
- subtraction from the left
- subtraction from the right
- division from the left
- division from the right

Number of ways:

1. choose 2 elements from 4                         (6 ways)
2. choose an operator and use it on those elements  (6 ways)
3. choose 2 elements from 3                         (3 ways)
4. choose an operator and use it on those elements  (6 ways)
5. now, there are only 2 elements left. choose op   (6 ways)

total number of ways = 3 * 6^4 = 3888
-}

import Data.Function (on)
import Data.List (maximumBy)

-- all possible ways to choose (k) numbers in the range [min, max], preserving order.
chooseNums :: Int -> (Int, Int) -> [[Int]]
chooseNums k (min, max)
	| min > max || k == 0 = [[]]
	| otherwise = concat [map (a:) $ chooseNums (k - 1) (a + 1, max) |
		a <- [min .. max - k + 1]]

-- 6 arithmetic operations, including order.
ops :: [Double -> Double -> Double]
ops = [(+), (*), (-), flip (-), (/), flip (/)]

-- given a list of Doubles, returns all possible ways to perform 1 arithmetic operation
-- on any 2 of its elements.
allOps :: [Double] -> [[Double]]
allOps ds = [f (ds !! i) (ds !! j) : remove i j (zip [0..] ds) |
	[i, j] <- chooseNums 2 (0, length ds - 1), f <- ops]
	where
		-- given a list zipped with its indices, removes the elements with
		-- indices (a) and (b).
		remove :: Int -> Int -> [(Int, Double)] -> [Double]
		remove _ _ [] = []
		remove a b ((index, x) : xs)
			| index == a || index == b = rest
			| otherwise = x : rest
			where rest = remove a b xs

-- all numbers expressible by arithmetic operations on the list.
allNumsExpressible :: [Double] -> [[Double]]
allNumsExpressible ds = until ((== 1) . length . head) (concatMap allOps) [ds]

-- input: input from (allNumsExpressible)
-- output: highest (n) such that [1, 2, ..., n] are in input
highestIntegerInARow :: [[Double]] -> Double
highestIntegerInARow xs = pred $ head $ filter (`notElem` map head xs) [1..]

main :: IO ()
main = print $ maximumBy (compare `on` snd) $ map mapF digitQuadruples
	where
		digitQuadruples = map (map fromIntegral) $ chooseNums 4 (1, 9)
		mapF quad = (quad, highestIntegerInARow $ allNumsExpressible quad)