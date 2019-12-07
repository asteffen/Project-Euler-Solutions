import Data.List (find, nub)
import Data.Numbers.Primes (primes)
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

-- Given a list length (n), returns all pairs of distinct indices of the list.
allPairs :: Int -> [(Int, Int)]
allPairs n = [(a, b) | a <- [0 .. n - 1], b <- [a + 1 .. n - 1]]

-- Returns all subsets of the set {0, 1, ..., n}.
allSubsets 0 = [[], [0]]
allSubsets n = rest ++ map (n :) rest
	where rest = allSubsets $ n - 1

-- Partitions a list into two lists, (ys) and (ns).
-- (ys) contains all the elements at the indices in (is).
-- (ns) contains all the elements at the indices not in (is).
sepIndices :: [Int] -> [a] -> ([a], [a])
sepIndices is xs = (\(ys, ns) -> (reverse ys, reverse ns)) $ sepIndices' 0 [] [] is xs

sepIndices' :: Int -> [a] -> [a] -> [Int] -> [a] -> ([a], [a])
sepIndices' curIndex ys ns [] [] = (ys, ns)
sepIndices' curIndex ys ns [] xs = (ys, reverse xs ++ ns)
sepIndices' curIndex ys ns ii@(i:is) (x:xs)
	| curIndex == i = sepIndices' (curIndex + 1) (x:ys) ns is xs
	| otherwise     = sepIndices' (curIndex + 1) ys (x:ns) ii xs

-- Returns all partitions of the list into two smaller sets.
-- (as, bs) and (bs, as) are considered distinct.
elemPartitions :: [a] -> [([a], [a])]
elemPartitions xs = map (\is -> sepIndices is xs) indexSets
	where indexSets = map reverse $ allSubsets $ length xs - 1

--factorPart [x] = [x]
--factorPart (x:xs) = 