import Data.Numbers.Primes (primes)
import Time

smallestN_old :: Integer -> Integer -> Integer
smallestN_old p1 p2 = head $ filter (\n -> n `mod` p2 == 0) [p1, p1 + inc ..]
	where inc = 10 ^ length (show p1)

-- Input: 2 Integers (a) and (b)
-- Output: the solution (x, y) to the equation (ax + by = gcd a b)
extendedGCD :: Integer -> Integer -> (Integer, Integer)
extendedGCD a b
	| r == 0    = (0, 1)
	| otherwise = (y, x - y * q)
	where
		(x, y) = extendedGCD b r
		(q, r) = a `divMod` b

-- Input: 2 Integers (a) and (m). they must be coprime for the output to be
--     correct.
-- Output: (a ^ -1), the modular multiplicative inverse of (a) mod (m).
modInv :: Integer -> Integer -> Integer
modInv m a
	-- | gcd a m /= 1 = error "modInv: not coprime"
	| otherwise    = fst $ extendedGCD a m

{-
Numbers such that the last digits are formed by p1 are of the form
	n = p1 + k * inc,
where
	inc = 10 ^ length (show p1)

We want to find the (n) such that (n `mod` p2 == 0).
	n = 0                (mod p2)
	p1 + k * inc = 0     (mod p2)
	k * inc = (-p1)      (mod p2)
	k = inc ^ -1 * (-p1)   (mod p2)

So the (k) that produces the lowest (n) that satisfies the above is
	k = modInv p2 inc * (-p1) `mod` p2
-}
smallestN :: Integer -> Integer -> Integer
smallestN p1 p2 = p1 + k * inc
	where
		inc = 10 ^ length (show p1)
		k = modInv p2 inc * (-p1) `mod` p2

consecPrimes :: [(Integer, Integer)]
consecPrimes = zip (takeWhile (< 1000000) $ drop 2 primes) $ drop 3 primes

main :: IO ()
main = timePrint $ sum $ map (uncurry smallestN) $ consecPrimes