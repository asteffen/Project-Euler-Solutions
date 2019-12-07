import Euler (factorTD)
import Data.List (nub)

rad :: Integer -> Integer
rad = product . nub . factorTD

-- Returns all the abc-hits with the given c-value.
abcHit :: Integer -> [(Integer, Integer, Integer)]
abcHit c = [(a, b, c) |
	a <- [1 .. (c - 1) `div` 2],
	let b = c - a,
	gcd a b == 1, gcd a c == 1, gcd b c == 1,
	rad (a * b * c) < c
	]

-- The sum of all c-values in the abc-hits such that c < n.
sumC :: Integer -> Integer
sumC n = sum $ map (\(_, _, c) -> c) $ concat $ map abcHit [1 .. n - 1]