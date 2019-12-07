{-
x = number of hexadecimal digits
y = amount of numbers that qualify

x | y
3 | 4
4 | 258
5 | 9928
6 | 299490
-}

import Data.Char (toUpper)
import Numeric (showHex)
import Time

--------------------------------------------------------------------------------
-- Brute force implementation for checking
--------------------------------------------------------------------------------

qualifies :: String -> Bool
qualifies hexStr = all (`elem` hexStr) "01a"

-- The amount of hexadecimal numbers with (len) digits that satisfy the rule.
numWays' :: Integer -> Int
numWays' len = length $ filter (qualifies . toHex) [16 ^ (len - 1) .. 16 ^ len - 1]

--------------------------------------------------------------------------------
-- Better algorithms
--------------------------------------------------------------------------------

-- Converts an Integer to a hexadecimal string. Uses lowercase letters.
toHex :: Integer -> String
toHex n = showHex n ""

-- Lists all the ways to express (n) as the sum of any amount of positive integers.
partitions :: Integer -> [[Integer]]
partitions n
	| n < 0 = []
	| n == 0 = [[]]
	| otherwise = concatMap (\k -> map (k :) $ partitions $ n - k) [1..n]

choose :: Integer -> Integer -> Integer
n `choose` k = product [n - k + 1 .. n] `div` product [1..k]

{-
(numWaysRep) does most of the work in the program... here is a sloppy explanation.

(parts) is a list of all triplets that represent
	[number of 0s, number of 1s, number of As]
that will be used in the number.

(numWaysPart) returns the number of ways a hexadecimal number could have the
specified counts of 0s, 1s, and As.
given: element of parts = [num0,. num1, numA]
it calculates the number of ways to place the 0s first by calculating
	((len - 1) `choose` num0).
(len - 1) is used becuase the first digit cannot be a 0.
it then calculates the number of ways to place the 1s and As by choosing
the specified amount from the remaining availible places.

the number of ways for each partition in (parts) is summed.
finally, this sum is multiplied by
	(13 ^ (len - num))
which is the amount of possibilities to fill the remaining spaces
with digits that are not 0, 1, or A.
-}
numWaysRep :: Integer -> Integer -> Integer
numWaysRep len num = sum (map numWaysPart parts) * 13 ^ (len - num)
	where
		parts = filter ((== 3) . length) $ partitions num
		numWaysPart [num0, num1, numA] = ways0 * ways1 * waysA
			where
				ways0 = (len - 1) `choose` num0
				ways1 = (len - num0) `choose` num1
				waysA = (len - num0 - num1) `choose` numA

-- The amount of hexadecimal numbers with (len) digits that satisfy the rule.
numWays :: Integer -> Integer
numWays len = sum $ map (numWaysRep len) [3..len]

main :: IO ()
main = timePrint $ map toUpper $ toHex $ sum $ map numWays [3..16]