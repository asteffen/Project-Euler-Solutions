-- 63.78 sec

import Data.Char (digitToInt, intToDigit)

-- Prime numbers, as an infinite list.
primes :: [Integer]
primes = 2 : filter is_prime [3,5..]

-- Mutually recursive with the primes list.
is_prime :: Integer -> Bool
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

-- returns all rotations of a list
rotations :: [a] -> [[a]]
rotations xs = take (length xs) $ iterate rotate xs
	where
		rotate :: [a] -> [a]
		rotate (x:xs) = xs ++ [x]

-- returns all rotations of an Integer
intRotations :: Integer -> [Integer]
intRotations = map digitsToInt . rotations . intToDigits

-- returns if the integer is a circular prime
isCircularPrime :: Integer -> Bool
isCircularPrime = all is_prime . intRotations

main :: IO ()
main = print $ length $ filter isCircularPrime [2..1000000]