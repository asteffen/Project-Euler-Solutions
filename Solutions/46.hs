-- 12.56 sec

-- Prime numbers, as an infinite list.
primes :: [Integer]
primes = 2 : filter is_prime [3,5..]

-- Mutually recursive with the primes list.
is_prime :: Integer -> Bool
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

-- infinite list of odd composite numbers
oddComposites :: [Integer]
oddComposites = filter (not . is_prime) [1,3..]

-- infinite list of 2n^2, natural number n
twiceSquares :: [Integer]
twiceSquares = [2 * n * n | n <- [1..]]

-- takes an increasing integer list up to a certain integer
upTo :: [Integer] -> Integer -> [Integer]
xs `upTo` n = takeWhile (<= n) xs

-- returns a list of (p, ts) such that
-- 1) p is prime
-- 2) ts is twice a square
-- 3) p + ts == n
conjecture :: Integer -> [(Integer, Integer)]
conjecture n = [(p, ts) | p <- primes `upTo` n, ts <- twiceSquares `upTo` n,
	p + ts == n]

-- returns (True) if an integer is a counterexample to the conjecture
isCounterEx :: Integer -> Bool
isCounterEx = null . conjecture

main :: IO ()
main = print $ head $ filter isCounterEx oddComposites