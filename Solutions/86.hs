-- found answer by experimenting with (numIntDistance) function.

-- pythagorean triples where (a) and (b) are less than a specified limit (n).
pythag :: Integer -> [[Integer]]
pythag n = [[a, b, c] | a <- [1..n], b <- [a+1..n],
	let (c, fracPart) = properFraction $ sqrt $ fromIntegral $ a * a + b * b,
	fracPart == 0]

-- input: a pythagorean triplet [a, b, c], and an integer n
-- output: the number of ways you can split up (b) into (b1 + b2) such that
-- both (b1) and (b2) are less than (a) and (n).
numSplitsOrdered :: Integer -> [Integer] -> Int
numSplitsOrdered n [a, b, c] = length $ filter filterF $
	map (\n -> (n, b - n)) [1 .. b `div` 2]
	where filterF (b1, b2) = max b1 b2 <= a && a <= n

-- same as (numSplitsOrdered), but calculates again with (a) and (b) switched
-- and sums the two amounts.
numSplits :: Integer -> [Integer] -> Int
numSplits n [a, b, c] = numSplitsOrdered n [a, b, c] + numSplitsOrdered n [b, a, c]

-- input: M, the maximum for each dimension of a cuboid
-- output: the number of cuboids with dimensions in the range [1..M] such that
-- the shortest distance between opposite vertices is an integer.
numIntDistance :: Integer -> Int
numIntDistance n = sum $ map (numSplits n) $ pythag $ 2 * n