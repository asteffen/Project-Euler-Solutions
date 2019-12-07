{-
expression evaluated:
main = timePrint $ length $ filter ((== 1) . numWays) $ range

range                             | value | time (evaluated -O)
[2, 4 .. 2*10^5]                  | 21964 | 126.109375 sec
[2*10^5+2, 2*10^5+4 .. 3*10^5]    | 10966 | 152.515625 sec
[3*10^5+2, 3*10^5+4 .. 5*10^5]    | 21516 | 494.921875 sec
[5*10^5+2, 5*10^5+4 .. 7*10^5]    | 21337 | 742.671875 sec
[7*10^5+2, 7*10^5+4 .. 9*10^5]    | 21354 | 994.890625 sec
[9*10^5+2, 9*10^5+4 .. 11*10^5]   | 21482 | 1233.671875 sec
[11*10^5+2, 11*10^5+4 .. 13*10^5] | 21545 | 1479.625000 sec
[13*10^5+2, 13*10^5+4 .. 15*10^5] | 21503 | 1723.093750 sec

total value = 161667
-}

import Data.Array
import Time

-- Infinite list of Primitive Pythagorean Triples (PPTs).
-- http://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
ppt :: [[Integer]]
ppt = [[a, b, c] | m <- [1..],
	n <- [1 .. m - 1], -- n < m.
	odd $ m + n,       -- exactly one of (m, n) is even.
	gcd m n == 1,      -- m and n are coprime.
	let
		m2 = m * m
		n2 = n * n
		a = m2 - n2
		b = 2 * m * n
		c = m2 + n2]

-- Perimeters of the Primitive Pythagorean Triples. Generally is an increasing
-- list, but sometimes an element is greater than the one before it.
perims :: [Integer]
perims = map sum ppt

divides :: Integer -> Integer -> Bool
d `divides` n = n `mod` d == 0

-- Returns the number of ways that a wire of length (n) can be bent for form
-- an integer-sided right triangle.
-- (2 * n) is an arbitrary bound that seems to work.
numWays :: Integer -> Int
numWays n = length $ filter (`divides` n) $ takeWhile (<= 2 * n) perims

-- new code start

lim :: Integer
lim = floor $ 1.5e6

numWaysArray :: Array Integer Integer
numWaysArray = listArray (1, lim) $ repeat 0

newAssocs :: Array Integer Integer -> Integer -> Array Integer Integer
newAssocs acc perim = acc // map (\i -> (i, acc ! i + 1)) indices
	where indices = takeWhile (<= lim) $ map (* perim) [1..]

newArray perimeters = foldl newAssocs numWaysArray perimeters

-- new code end

main :: IO ()
main = timePrint $ length $ filter ((== 1) . numWays) $ [13*10^5+2, 13*10^5+4 .. 15*10^5]
--main = print $ length $ filter (== 1) $ elems $ newArray perims