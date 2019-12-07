-- takes a whopping 191 seconds to run

import Data.List (permutations)
import Data.Char (digitToInt, intToDigit)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

take3atIndex :: Int -> [a] -> [a]
take3atIndex i xs = map (xs !!) [i..i+2]

hasProperty :: Integer -> Bool
hasProperty n = and $ zipWith zipFunc subNums divisors
	where
		zipFunc subNum divisor = subNum `mod` divisor == 0
		-- [1..7] because list[1] is the 2nd digit, list[7] is the 8th digit
		subNums = map (\i -> digitsToInt $ take3atIndex i $ intToDigits n) [1..7]
		divisors = [2, 3, 5, 7, 11, 13, 17]

pandigitals :: [Integer]
pandigitals = [digitsToInt p | p <- permutations [0..9], head p /= 0]

main :: IO ()
main = print $ sum $ filter hasProperty pandigitals

--------------------------------------------------------------------------------
-- Much faster, but ugly
--------------------------------------------------------------------------------

qualifiers :: [Integer]
qualifiers = [digitsToInt [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10] |
	d1 <- [1..9],
	d2 <- [0..9], d2 /= d1,
	d3 <- [0..9], d3 /= d1, d3 /= d2,
	d4 <- [0..9], d4 /= d1, d4 /= d2, d4 /= d3,
	(100*d2 + 10*d3 + d4) `mod` 2 == 0,
	d5 <- [0..9], d5 /= d1, d5 /= d2, d5 /= d3, d5 /= d4,
	(100*d3 + 10*d4 + d5) `mod` 3 == 0,
	d6 <- [0..9], d6 /= d1, d6 /= d2, d6 /= d3, d6 /= d4, d6 /= d5,
	(100*d4 + 10*d5 + d6) `mod` 5 == 0,
	d7 <- [0..9], d7 /= d1, d7 /= d2, d7 /= d3, d7 /= d4, d7 /= d5, d7 /= d6,
	(100*d5 + 10*d6 + d7) `mod` 7 == 0,
	d8 <- [0..9], d8 /= d1, d8 /= d2, d8 /= d3, d8 /= d4, d8 /= d5, d8 /= d6, d8 /= d7,
	(100*d6 + 10*d7 + d8) `mod` 11 == 0,
	d9 <- [0..9], d9 /= d1, d9 /= d2, d9 /= d3, d9 /= d4, d9 /= d5, d9 /= d6, d9 /= d7, d9 /= d8,
	(100*d7 + 10*d8 + d9) `mod` 13 == 0,
	d10 <- [0..9], d10 /= d1, d10 /= d2, d10 /= d3, d10 /= d4, d10 /= d5, d10 /= d6, d10 /= d7, d10 /= d8, d10 /= d9,
	(100*d8 + 10*d9 + d10) `mod` 17 == 0
	]

main' :: IO ()
main' = print $ sum $ qualifiers