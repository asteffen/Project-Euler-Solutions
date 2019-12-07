import Data.List (group)

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

-- infinite list of triangular numbers
triangular_nums :: [Integer]
triangular_nums = [ n * (n+1) `div` 2 | n <- [1..] ]

-- the number of divisors in a number, including 1 and itself.
-- if a number (n) has prime factorization p_1^(e_1) * p_2^(e_2) * ... * p_n^(e_n),
-- then it has (e_1 + 1) * (e_2 + 1) * ... * (e_3 + 1) factors.
num_divisors :: Integer -> Integer
num_divisors = product . map ((+1) . fromIntegral . length) . group . factorList

main :: IO ()
main = print $ head $ dropWhile (\n -> num_divisors n <= 500) triangular_nums