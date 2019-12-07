import Data.List (nub, sort)

type BinOp = Double -> Double -> Double

-- replaces [x_i, x_(i+1)] with [f x_i x_(i+1)].
reduce :: [Double] -> Int -> BinOp -> [Double]
reduce dList i f = map snd $ substitute $ zip [0..] dList
	where
		substitute :: [(Int, Double)] -> [(Int, Double)]
		substitute [] = []
		substitute (x@(index, dbl) : xs)
			| index == i   = (index, f (dList !! i) (dList !! (i+1))) : rest
			| index == i+1 = rest
			| otherwise    = x : rest
			where rest = substitute xs

-- concatenate two doubles together.
concatNums :: BinOp
concatNums a b = fromIntegral $ read $ a' ++ b'
	where [a', b'] = map (show . round) [a, b]

-- all subsets of (xs).
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) rest ++ rest
	where rest = subsets xs

-- given a subset (is), concatenates between each index pair (i, i + 1) in (xs).
eval :: [Int] -> [Double] -> [Double]
eval [] xs = xs
eval (i:is) xs = eval (map pred is) $ reduce xs i concatNums

-- all possible concatenations of [1..9].
allConcats :: [[Double]]
allConcats = map (\is -> eval is [1..9]) $ subsets [0..7]

-- all ways of applying a binary operation on a pair of adjacent doubles.
apply :: [Double] -> [[Double]]
apply xs = [reduce xs i f | i <- [0 .. length xs - 2], f <- [(+), (-), (*), (/)]]

-- to be counted in the sum, a double must be a positive integer.
isEligible :: Double -> Bool
isEligible x = x > 0 && snd (properFraction x) == 0