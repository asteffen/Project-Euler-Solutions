-- 27 sec

import Data.List (tails, maximumBy)
import Data.Function (on)

-- All primes (excluding 2 and 3) are of the form (6k +- 1),
-- where k is an integer >= 1.
primes :: [Integer]
primes = 2 : 3 : filter isPrime (concat [[k - 1, k + 1] | k <- [6,12..]])

-- Mutually recursive with (primes).
isPrime :: Integer -> Bool
isPrime p = all ((/= 0) . (p `mod`)) $ takeWhile (\n -> n * n <= p) primes

-- returns (# of consecutive primes, sum), such that:
-- (# of consecutive primes) is maximized, and sum < n.
mostConsecPrimes :: Integer -> (Int, Integer)
mostConsecPrimes n = maximumBy (compare `on` fst) $ filter (isPrime . snd) $
	concatMap (zip [2..] . tail) consecSums
	where
		-- list of lists of consecutive sums of primes starting at each prime,
		-- where every sum is below (n). ex:
		-- [[2, 5, 10, ...], [3, 8, 15, ...], [5, 12, 23, ...], ...]
		consecSums = takeWhile ((> 1) . length) $
			map (takeWhile (< n) . scanl1 (+)) $ tails primes

main :: IO ()
main = print $ snd $ mostConsecPrimes 1000000