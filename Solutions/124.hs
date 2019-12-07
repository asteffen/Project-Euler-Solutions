-- interpreted: 43 sec

import Data.List (find, nub, sortBy)
import Data.Numbers.Primes (primes)
import Data.Function (on)
import Time

-- Returns (True) if d is a factor of n.
divides :: Integer -> Integer -> Bool
d `divides` n = n `mod` d == 0

-- Completely factorize an integer using trial division.
-- When performing trial division, also checks for primality, to increase speed.
factorTD :: Integer -> [Integer]
factorTD n
	| n == 1    = []
	| otherwise = case find (`divides` n) $ takeWhile (\p -> p * p <= n) primes of
		Nothing -> [n] -- n is prime.
		Just d  -> d : factorTD (n `div` d)

rad :: Integer -> Integer
rad = product . nub . factorTD

sorted :: [Integer]
sorted = sortBy (compare `on` rad) [1..10^5]

main :: IO ()
main = timePrint $ sorted !! 9999