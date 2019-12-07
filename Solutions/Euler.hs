module Euler where

import Data.Char (digitToInt, intToDigit)
import Data.List (find, genericReplicate)
import Data.Numbers.Primes (primes)
import Data.Array.ST (runSTUArray, STUArray)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.Unboxed (assocs, UArray)
import Data.Array.Base (unsafeWrite, unsafeRead, newArray)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: (Integral a) => a -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: (Integral a, Read a) => [Int] -> a
digitsToInt = read . map intToDigit

isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
isPrime p = all ((/= 0) . (p `mod`)) $ takeWhile (\n -> n * n <= p) primes

-- Completely factorize an integer using trial division.
-- When performing trial division, also checks for primality, to increase speed.
factorTD :: Integer -> [Integer]
factorTD n
	| n == 1    = []
	| otherwise = case find (`divides` n) $ takeWhile (\p -> p * p <= n) primes of
		Nothing -> [n] -- n is prime.
		Just d  -> d : factorTD (n `div` d)

-- Returns the floor of the square root of (n). Works for large integers.
-- From http://darcs.brianweb.net/hsutils/Doc.pdf
intSqrt :: Integer -> Integer
intSqrt n
	| n < 0 = error "intSqrt: negative n"
	| n == 0 = 0
	| otherwise = f n
	where
		f x = if y < x then f y else x
			where y = (x + (n `quot` x)) `quot` 2

-- Returns (True) if d is a factor of n.
divides :: Integer -> Integer -> Bool
d `divides` n = n `mod` d == 0

-- Returns (True) if the argument is prime. Uses trial division on the primes 
-- from 1 to (sqrt p).
isPrimeTD :: Integer -> Bool
isPrimeTD p
	| p <= 1    = False
	| otherwise = all notDiv $ takeWhile (\n -> n * n <= p) primes
		where notDiv = not . (`divides` p)

---------- new

-- primitive pythagorean triples
ppt :: [[Integer]]
ppt = [[m * m - n * n, 2 * m * n, m * m + n * n] |
	m <- [2..],
	n <- [m-1, m-3 .. 1],
	gcd m n == 1]

{-
Interesting math fact

the number of ways to express (b) as the sum of (not necessarily
distinct) elements in the set {a_n}
= ways b [a1, a2, a3, ..., an]
= the coefficient of (x^b) in the taylor expansion of
1 / ((1 - x^a1) * (1 - x^a2) * (1 - x^a3) * ... * (1 - x^an))
-}

-- e.g. number of ways to make change for a dollar
ways :: (Integral a) => a -> [a] -> [[a]]
ways y [] = []
ways y [x]
	| m == 0 = [genericReplicate d x]
	| otherwise = []
	where (d, m) = y `divMod` x
ways y (x:xs) = concat $ map f [0 .. y `div` x]
	where f a = map (genericReplicate a x ++) $ ways (y - a*x) xs

-- more efficient version of (length . ways)
numWays :: (Integral a) => a -> [a] -> a
numWays y [] = 0
numWays y [x]
	| y `mod` x == 0 = 1
	| otherwise = 0
numWays y (x:xs) = sum $ map (\a -> numWays (y - a*x) xs) [0 .. y `div` x]

{-
Note: (unsafeWrite) and (unsafeRead) are about twice as fast as
(writeArray) and (readArray).
-}

-- faster than Data.Numbers.Primes.primes
-- makes an array of primes up to lim^2
sieveArray :: Int -> UArray Int Bool
sieveArray lim = runSTUArray (do
	ar <- newArray (0, lim * lim) True
	let go i = do
		b <- unsafeRead ar i
		when b (do
			let run j
				| j > lim * lim = return ()
				| otherwise = do
					c <- unsafeRead ar j
					when c $ unsafeWrite ar j False
					run $ j + i
			run $ i * i)
	
	unsafeWrite ar 0 False
	unsafeWrite ar 1 False
	mapM_ go [2 .. lim - 1]
	return ar)

{-
-- this code is easier to understand, but slightly slower.
sieveArray :: UArray Int Bool
sieveArray = runSTUArray (do
	ar <- newArray (0, lim*lim) True
	let write = unsafeWrite ar
	let look  = unsafeRead ar
	let markFalse i = do
		c <- look i
		when c $ write i False
	let go i = do
		b <- look i
		when b $ mapM_ markFalse [i*i, i*i+i .. lim*lim]
	
	write 0 False
	write 1 False
	mapM_ go [2 .. lim - 1]
	return ar)
-}

sieveList :: Int -> [Int]
sieveList lim = [p | (p, True) <- assocs $ sieveArray lim]