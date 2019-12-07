import Data.List (maximumBy, splitAt)

-- Prime numbers, as an infinite list.
primes :: [Integer]
primes = 2 : filter is_prime [3,5..]

-- Mutually recursive with the primes list.
is_prime :: Integer -> Bool
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

-- Returns the number k, where k is the highest integer such that
-- every number produced by (n^2 + a*n + b) is prime for n in [0..k].
primes_produced :: Integer -> Integer -> Int
primes_produced a b = length $ takeWhile is_prime_and_positive [n*n + a*n + b | n <- [0..]]
	where is_prime_and_positive p = p > 0 && is_prime p

{-
I needed to split (produced) into two lists, becuase when I tried to do
(maximumBy max_func producued), I got an error because of stack overflow.

The program takes about 27 sec to run.
-}
main = print $ product_fst_2 $ max_third [max_third p1, max_third p2]
	where
		produced = [(a, b, primes_produced a b) | a <- abs1000, b <- prime1000]
		abs1000 = [-1000..1000]
		prime1000 = takeWhile (<1000) primes
		max_func (_, _, k1) (_, _, k2) = compare k1 k2
		max_third = maximumBy max_func
		product_fst_2 (a, b, _) = a * b
		(p1, p2) = splitAt (length produced `div` 2) produced