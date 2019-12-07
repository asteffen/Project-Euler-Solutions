-- 8.8 sec

import Time

maxPerim :: Integer
maxPerim = 10^8

-- all primitive pythagorean triples, and their sum
-- the sums are not in increasing order
ppt :: [[Integer]]
ppt = [[a, b, c, a + b + c] |
	m <- [2..],
	n <- [1..m-1],
	odd $ m + n,
	gcd m n == 1,
	let
		m2 = m * m
		n2 = n * n
		a = m2 - n2
		b = 2 * m * n
		c = m2 + n2]

-- property: if [a, b, c] qualifies, then so does [ka, kb, kc]
qual [a, b, c, _] = c `mod` (a - b) == 0

-- returns how many multiples of the triple have a perimeter under the maximum.
numMult [_, _, _, s] = maxPerim `div` s

-- used (1.5e8) as an estimate
-- all triples with perimeter under 1e8 should come before it
ans = sum $ map numMult $ filter qual $ takeWhile ((< floor 1.5e8) . last) ppt

main = timePrint $ ans