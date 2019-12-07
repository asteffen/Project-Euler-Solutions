-- returns the ways to express (n) as the sum of any amount of the elements in (xs).
-- putting the list in descending order makes it run faster.
-- This function is taken from 31.hs
ways_to_make :: (Integral a) => a -> [a] -> a
ways_to_make n (x:xs)
	| xs == []  = if n `mod` x == 0 then 1 else 0
	| otherwise = sum $ map (\r -> ways_to_make r xs) remainders
	where remainders = takeWhile (>=0) $ iterate (subtract x) n

-- Prime numbers, as an infinite list.
primes :: [Integer]
primes = 2 : filter is_prime [3,5..]

-- Mutually recursive with the primes list.
is_prime :: Integer -> Bool
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

-- returns the number of ways to partition (n) into a sum of primes
-- (not counting order). reverses the partial primes list for better performance.
partitionPrimes :: Integer -> Integer
partitionPrimes n = ways_to_make n $ reverse $ takeWhile (<=n) primes

main :: IO ()
main = print $ head $ filter (\n -> partitionPrimes n > 5000) [2..]