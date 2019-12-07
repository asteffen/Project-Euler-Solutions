
{-
Alternate way of thinking about it: placing black squares

you are allowed to skip places, but only 3 consecutive times or more.
-}

import Time

-- number of ways with adding a red block at the front of length k (template n spaces)
numWaysRed n k = numWays $ n - k - 1

-- number of ways, not counting those with a black block at the first space
numWaysP n
	| n <= 1 = 1
	| otherwise = sum (map (numWaysRed n) [3..n]) -- + sum (map numWays [1..n-1])

numWays n
	| n <= 1 = 1
	| otherwise = sum (map (numWaysRed n) [3..n]) + sum (map numWaysP [1..n-1])

-- table indexing for efficiency
numWaysRedT n k = numWaysT $ n - k - 1

numWaysPT n
	| n <= 1 = 1
	| otherwise = nwpt !! n

nwpt = 0 : 0 : [sum $ map (numWaysRedT n) [3..n] | n <- [2..]]

numWaysT n
	| n <= 1 = 1
	| otherwise = nwt !! n

nwt = 0 : 0 : [sum (map (numWaysRedT n) [3..n]) + sum (map numWaysPT [1..n-1]) | n <- [2..]]

main = timePrint $ numWaysT 50