import Data.Numbers.Primes (primes)
import Data.List (sort)

divides :: Integer -> Integer -> Bool
d `divides` n = n `mod` d == 0

isPrime :: Integer -> Bool
isPrime p
	| p <= 1    = False
	| otherwise = all notDiv $ takeWhile (\n -> n * n <= p) primes
		where notDiv = not . (`divides` p)

--------------------------------------------------------------------------------
-- Trial division
--------------------------------------------------------------------------------

leastPrimeFactor :: Integer -> Integer
leastPrimeFactor n = head $ filter (`divides` n) primes

factor :: Integer -> [Integer]
factor 1 = []
factor n
	| isPrime n = [n]
	| otherwise = lpf : factor (n `div` lpf)
	where lpf = leastPrimeFactor n

--------------------------------------------------------------------------------
-- Pollard's rho algorithm
--------------------------------------------------------------------------------

factorRho :: Integer -> [Integer]
factorRho = sort . concatMap factor . factorRho'

-- Does not factor into prime factors.
factorRho' :: Integer -> [Integer]
factorRho' 1 = []
factorRho' n
	| isPrime n = [n]
	| otherwise = nextFactor : factorRho' (n `div` nextFactor)
	where nextFactor = pollardRho n

-- Wrapper for "pollard" function.
pollardRho :: Integer -> Integer
pollardRho = pollard 2 2 1

pollard :: Integer -> Integer -> Integer -> Integer -> Integer
pollard x y d n
	| d == 1 = pollard x' y' d' n
	| otherwise = d
	where
		g = (`mod` n) . f
		x' = g x
		y' = g $ g y
		d' = gcd (abs $ x' - y') n

f :: Integer -> Integer
f x = x^2 + 1