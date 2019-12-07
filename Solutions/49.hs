import Data.List (permutations, sort, nub, (\\))
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
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

contains0 :: Integer -> Bool
contains0 = any (== 0) . intToDigits

fourDigitPrimes :: [Integer]
fourDigitPrimes = filter is_prime [1000..10000]

-- distinct, increasing digit permutations of the integer
digitPerms :: Integer -> [Integer]
digitPerms = nub . sort . map digitsToInt . permutations . intToDigits

-- list of all a, b, c such that:
-- 1) a, b, c are permutations of the digits of n
-- 2) a, b, c are an increasing arithmetic sequence
permArith :: Integer -> [[Integer]]
permArith n = [[a, b, c] | a <- perms, b <- perms, a < b,
	let c = 2*b - a, c `elem` perms]
	where perms = digitPerms n

-- same as above but with a third requirement:
-- 3) a, b, c are prime
permArithPrimes :: Integer -> [[Integer]]
permArithPrimes = filter (all is_prime) . permArith

main = print $ concatInts otherSeq
	where
		-- returns if there exists an increasing arithmetic sequence,
		-- where each term is a permutation of (n) and is prime.
		-- And also that n does not contain the digit 0.
		qualifies n = (not . contains0) n && (not . null . permArithPrimes) n
		
		-- a number containing the digits of the other sequence besides 1487.
		otherNum = head $ filter qualifies fourDigitPrimes \\ digitPerms 1487
		
		-- the other sequence,
		-- made out of permutations of the digits of (otherNum)
		otherSeq = head $ permArithPrimes otherNum
		
		-- function that concatenates together a list of integers
		concatInts = digitsToInt . concatMap intToDigits