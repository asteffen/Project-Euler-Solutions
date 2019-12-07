{-
1/x + 1/y = 1/n
can be converted to a different formula:
(x - n)(y - n) = n^2

A much more efficient algorithm will be needed to solve this problem...
4 million solutions is a lot.

The following code runs in 64 sec and outputs 1146353. (1.1 million)
length $ findSols3 $ product [1..20]
-}

import Euler (divides, factorTD)
import Data.List (sort)

-- Given (n), returns the solutions (x, y) to (1/x + 1/y == 1/n).
findSols :: Integer -> [(Integer, Integer)]
findSols n = [(x, y) | x <- [n + 1 .. 2 * n],
	let (y, m) = (n * x) `divMod` (x - n), m == 0]

-- faster than (findSols). Finds the divisors by dividing (n^2) by all integers
-- from 1 to n.
findSols2 :: Integer -> [(Integer, Integer)]
findSols2 n = map divisorToPair divs
	where
		n2 = n * n
		divs = divisors n2 [1..n]
		divisorToPair d = (d + n, n2 `div` d + n)

-- Returns all the positive integer divisors of the argument.
divisors :: Integer -> [Integer] -> [Integer]
divisors _ [] = []
divisors p (x:xs)
	| x `divides` p = x : rest
	| otherwise     = rest
	where rest = divisors p xs

-- faster than (findSols2). Finds the divisors by prime factorization.
findSols3 :: Integer -> [(Integer, Integer)]
findSols3 n = map divisorToPair divs
	where
		n2 = n * n
		divs = takeWhile (<= n) $ divisors2 n2
		divisorToPair d = (d + n, n2 `div` d + n)

-- returns a list of all the positive integer divisors of the argument.
divisors2 :: Integer -> [Integer]
divisors2 = sort . multiplyCombs . factorTD

-- input: prime factors of an integer
-- output: all divisors of the integer (not in increasing order)
multiplyCombs :: [Integer] -> [Integer]
multiplyCombs [] = [1]
multiplyCombs (curFact : factors) = [x * y | x <- choices, y <- multiplyCombs rest]
	where
		(curGroup, rest) = span (== curFact) factors
		choices = take (length curGroup + 2) $ iterate (* curFact) 1

numSols :: Integer -> Int
numSols n = length $ takeWhile (<= n) $ divisors2 $ n * n