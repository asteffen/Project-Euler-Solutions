{-
How I found the answer:

I was testing with the allNumPrimes function (slow).

First I found that (allNumPrimes 109 == 8). But this did't count because
one of the primes counted was 109, or 000109.

I found the answer by doing (map allNumPrimes [201..300]).
(allNumPrimes 233 == 8). By using the np function, I found these primes were:
121313, 222323, 323333, 424343, 525353, 626363, 828383, 929393
The answer is the lowest of these, 121313.
-}

import Data.Numbers.Primes (primes)
import Data.Char (digitToInt, intToDigit)
import Data.List (nub)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

isPrime :: Integer -> Bool
isPrime p = all ((/= 0) . (p `mod`)) $ takeWhile (\n -> n * n <= p) primes

-- not necessarily a correct function, since it is possible there could
-- be a number group where the amount of numbers replaced is greater than
-- the amount that remain the same.
allNumPrimes :: Integer -> Int
allNumPrimes n = maximum $ map (numPrimes n) [1..length $ intToDigits n]

-- maximum number of primes possible by inserting (k) of the same digit
-- into any place in the digits of (n).
numPrimes :: Integer -> Int -> Int
numPrimes n k = maximum $ map (length . filter isPrime . substitute) template
	where
		-- in this template, (-1) means *.
		template = nub $ insert (replicate k (-1)) (intToDigits n)
		
		-- substitutes (-1) by each of [0..9]
		substitute xs = map (digitsToInt . replaceAll xs (-1)) [0..9]

-- quick function used to find the actual primes in one of the families,
-- instead of just the number that there are.
np n k = map (filter isPrime . substitute) template
	where
		-- in this template, (-1) means *.
		template = nub $ insert (replicate k (-1)) (intToDigits n)
		
		-- substitutes (-1) by each of [0..9]
		substitute xs = map (digitsToInt . replaceAll xs (-1)) [0..9]

-- returns a list of all combinations resulting from inserting
-- each element from list xs into list ys (in any order).
insert :: [a] -> [a] -> [[a]]
insert xs ys = foldr (\x acc -> concatMap (put x) acc) [ys] xs

-- returns a list of all combinations resulting from inserting
-- x somewhere in list ys.
put :: a -> [a] -> [[a]]
put x [] = [[x]]
put x yy@(y:ys) = (x : yy) : map (y:) (put x ys)

-- replaces all instances of (old) in a (xs) with (new).
replaceAll :: (Eq a) => [a] -> a -> a -> [a]
replaceAll [] _ _ = []
replaceAll (x:xs) old new
	| x == old  = new : rest
	| otherwise = x : rest
	where rest = replaceAll xs old new