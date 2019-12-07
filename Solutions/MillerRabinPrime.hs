-- http://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test#Deterministic_variants_of_the_test

module MillerRabinPrime where

-- modular exponentiation using binary exponentiation
modExp :: Integer -> Integer -> Integer -> Integer
modExp m a b
	| b == 0    = 1
	| mod2 == 0 = modExp m (a * a `mod` m) div2
	| otherwise = a * modExp m a (b - 1) `mod` m
	where (div2, mod2) = b `divMod` 2

-- input: s = 0, d = n - 1
-- output: (s, d)
factorPowersOf2 :: Integer -> Integer -> (Integer, Integer)
factorPowersOf2 s d
	| mod2 == 0 = factorPowersOf2 (s + 1) div2
	| otherwise = (s, d)
	where (div2, mod2) = d `divMod` 2

-- returns a list of the (a) values needed to test the primality of (n). see also:
-- http://www.research.att.com/~njas/sequences/A014233
aList :: Integer -> [Integer]
aList n
	| n < 2047 = [2]
	| n < 1373653 = [2, 3]
	| n < 9080191 = [31, 73]
	| n < 4759123141 = [2, 7, 61]
	| n < 2152302898747 = [2, 3, 5, 7, 11]
	| n < 3474749660383 = [2, 3, 5, 7, 11, 13]
	| n < 341550071728321 = [2, 3, 5, 7, 11, 13, 17]
	| otherwise = [2 .. floor $ 2 * (log $ fromIntegral n)^2]

-- Miller-Rabin primality test
mrPrime :: Integer -> Bool
mrPrime n
	| n == 2 = True
	| n < 2 || even n = False
	| otherwise = all aTest $ aList n
	where
		s, d :: Integer
		(s, d) = factorPowersOf2 0 (n - 1)
		
		-- returns (True) if, with the given (a) value, it is possible for (n) to be prime.
		aTest :: Integer -> Bool
		aTest a = modExp n a d == 1 || any rTest [0 .. s - 1]
			where
				rTest :: Integer -> Bool
				rTest r = modExp n a (2^r * d) == n - 1

---

main = print $ sum $ filter mrPrime [1000000000..1000010000]