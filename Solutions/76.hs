{-
The way I found the answer was with wikipedia.
The problem reminded me of the partition function, so I went to:

http://en.wikipedia.org/wiki/Partition_function_%28number_theory%29

I noticed in the "Table of values" section, p(100) = 190569292.
But since the problem is asking for the sum of 2 or more positive integers,
I subtracted 1 from this to get the answer: 190569291.
-}

-- the partition function, calculated using an intermediate function
-- (algorithm found on wikipedia page).
-- scales badly. (part 50) takes 6.84 seconds to evaluate.
part :: Integer -> Integer
part n = 1 + sum [p k (n-k) | k <- [1..n `div` 2]]
	where p k n -- intermediate function
		| k > n     = 0
		| k == n    = 1
		| otherwise = p (k+1) n + p k (n-k)

-- returns the ways to express (n) as the sum of any amount of the elements in (xs).
-- putting the list in descending order makes it run faster.
-- This function is taken from 31.hs
ways_to_make :: (Integral a) => a -> [a] -> a
ways_to_make n (x:xs)
	| xs == []  = if n `mod` x == 0 then 1 else 0
	| otherwise = sum $ map (\r -> ways_to_make r xs) remainders
	where remainders = takeWhile (>=0) $ iterate (subtract x) n

-- second version of partition function. uses ways_to_make function from problem 31.
-- faster than using intermediate function, but still scales badly.
-- (part2 50) takes 1.58s, (part2 60) takes 6.77s.
part2 :: Integer -> Integer
part2 n = ways_to_make n [n,n-1..1]