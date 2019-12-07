-- 28.02 sec

import Data.List (inits, tails)
import Data.Char (digitToInt, intToDigit)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

-- Prime numbers, as an infinite list.
primes :: [Integer]
primes = 2 : filter is_prime [3,5..]

-- Mutually recursive with the primes list.
is_prime :: Integer -> Bool
is_prime 1 = False
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

isTruncPrime :: Integer -> Bool
isTruncPrime n = all is_prime [digitsToInt $ map fromIntegral sub |
	sub <- inits digits ++ tails digits, not (null sub)]
	where digits = intToDigits n

main :: IO ()
main = print $ sum $ take 11 $ filter isTruncPrime [10..]