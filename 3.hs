-- Prime numbers, as an infinite list.
primes :: [Integer]
primes = 2 : filter is_prime [3,5..]

-- Mutually recursive with the primes list.
is_prime :: Integer -> Bool
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

-- list containing factors of an integer
factorList :: Integer -> [Integer]
factorList 1 = []
factorList n = leastPrimeFactor : factorList (n `div` leastPrimeFactor)
	where leastPrimeFactor = head $ filter (\k -> n `mod` k == 0) primes

main :: IO ()
main = putStrLn $ show $ last $ factorList 600851475143