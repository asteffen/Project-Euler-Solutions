{-
Takes 22 minutes to find the list
[8389,6733,5701,5197,13]
-}

import Data.Numbers.Primes (primes)
import Data.Char (digitToInt, intToDigit)
import Data.List (find)
--import MillerRabinPrime

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime p = all ((/= 0) . (p `mod`)) $ takeWhile (\n -> n * n <= p) primes

-- concatenate 2 numbers together
concatNums :: Integer -> Integer -> Integer
concatNums a b = digitsToInt $ intToDigits a ++ intToDigits b

primeConcat :: Integer -> Integer -> Bool
primeConcat a b = isPrime (concatNums a b) && isPrime (concatNums b a)

check :: [Integer] -> IO ()
check ps = do
	putStrLn $ "checking up to " ++ show (last first100)
	case find (not . null) $ map prime5less first100 of
		Just x -> putStrLn $ "found it: " ++ show x
		Nothing -> check rest
	where
		(first100, rest) = splitAt 100 ps

prime5less :: Integer -> [[Integer]]
prime5less n = [xs |
	let a = n,
	b <- takeWhile (< a) primes,
	primeConcat a b,
	c <- takeWhile (< b) primes,
	primeConcat a c,
	primeConcat b c,
	d <- takeWhile (< c) primes,
	primeConcat a d,
	primeConcat b d,
	primeConcat c d,
	e <- takeWhile (< d) primes,
	primeConcat a e,
	primeConcat b e,
	primeConcat c e,
	primeConcat d e,
	let xs = [a, b, c, d, e]]

main :: IO ()
main = check primes