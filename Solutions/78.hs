{-
runs in: 50-54 sec

I originally implemented (partitions) using a list, but the indexing was
too slow. The array implementation is much faster.
-}

import Data.List (find, genericTake)
import Data.Array

-- size of the (partitions) array.
arraySize :: Integer
arraySize = 100000

-- generalized pentagonal numbers.
genPentNums :: [Integer]
genPentNums = map (\n -> n * (3*n - 1) `div` 2) $ helper 1
	where helper n = n : -n : helper (n + 1)

-- partition function, mututally recursive with (partitions) array.
part :: Integer -> Integer
part n = sum $ zipWith zipFunc terms signs
	where
		terms = takeWhile (<= n) genPentNums
		signs = cycle [1, 1, -1, -1]
		zipFunc term sign = sign * partitions ! (n - term)

-- array of partitions. starts at p(0) = 1.
partitions :: Array Integer Integer
partitions = listArray (0, arraySize) $ 1 : map part [1..arraySize]

-- checks if any of the first (n) partition numbers are divisible by 1 million.
-- steps by 1000 to avoid stack overflow.
check :: Integer -> IO ()
check n = do
	putStrLn $ "Checking up to " ++ show n
	case find ((== 0) . (`mod` 1000000) . snd) $ genericTake n $ assocs partitions of
		Just (k, partK) -> putStrLn $ "Found it: p(" ++ show k ++ ") = " ++ show partK
		Nothing         -> check $ n + 1000

main :: IO ()
main = check 0
--main = print $ find ((== 0) . (`mod` 1000000) . snd) $ assocs partitions