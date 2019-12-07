-- 40 sec

{-
the diagonals of the spiral follow a pattern:

1, 3, 5, 7, 9, 13, 17, 21, 25, 31, 37, 43, 49, ...

the differences between successive terms are:

2, 2, 2, 2, 4, 4, 4, 4, 6, 6, 6, 6, ...
-}

-- (+2), (+2), (+2), (+2), (+4), (+4), (+4), (+4), ...
funcList :: [Integer -> Integer]
funcList = concatMap (replicate 4 . (+)) [2,4..]

-- applies each function in a list to get the next element
-- starts with an initial element.
applyFuncs :: [a -> a] -> a -> [a]
applyFuncs (f:fs) init = init : applyFuncs fs (f init)

spiralDiags :: [Integer]
spiralDiags = applyFuncs funcList 1

-- Prime numbers, as an infinite list.
primes :: [Integer]
primes = 2 : filter is_prime [3,5..]

-- Mutually recursive with the primes list.
is_prime :: Integer -> Bool
is_prime 1 = False
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

-- returns ratio for spiral with side length (n).
-- inefficient, used for verification.
percent n = fromIntegral (length (filter is_prime $ take k spiralDiags)) / fromIntegral k
	where k = (n - 1) `div` 2 * 4 + 1

-- a spiral with side length (2n+1) has (4n+1) numbers on its diagonals.
-- returns the lowest side length such that the ratio of
-- prime numbers on the diagonal is <= (ratio).
sideLength :: Double -> Int
sideLength ratio = sideLength' 1 0 spiralDiags
	where
	sideLength' :: Int -> Int -> [Integer] -> Int
	sideLength' n numPrimes curList
		| fromIntegral newNumPrimes <= ratio * (4*fromIntegral n + 1) = 2*n + 1
		| otherwise = sideLength' (n + 1) newNumPrimes drop4
		where
			(take4, drop4) = splitAt 4 curList
			newNumPrimes = numPrimes + (length $ filter is_prime take4)

main :: IO ()
main = print $ sideLength 0.1