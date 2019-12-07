-- 10.78 sec

import Data.Set (size, fromList)

-- Prime numbers, as an infinite list.
primes :: [Integer]
primes = 2 : filter is_prime [3,5..]

-- Mutually recursive with the primes list.
is_prime :: Integer -> Bool
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

-- prime squares, cubes, fouth powers
prime2, prime3, prime4 :: [Integer]
prime2 = takeWhile (< 50000000) $ map (^2) primes
prime3 = takeWhile (< 50000000) $ map (^3) primes
prime4 = takeWhile (< 50000000) $ map (^4) primes

-- numbers expressible as the sum of a prime square, prime cube, and prime fourth power.
-- note: some integers appear more than once in this list.
primeSums :: [Integer]
primeSums = [primeSum | p4 <- prime4, p3 <- prime3, p2 <- prime2,
	let primeSum = p2 + p3 + p4, primeSum < 50000000]

-- I'm using Data.Set functions here because when I tried to evaluate
-- (length $ Data.List.nub primeSums), the program would never finish.
main :: IO ()
main = print $ size $ fromList primeSums