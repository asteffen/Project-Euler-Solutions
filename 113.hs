import Data.Char (digitToInt)

--------------------------------------------------------------------------------
-- Brute-force functions for testing
--------------------------------------------------------------------------------

-- converts an integer into a list of digits, e.g. 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

isIncreasing :: Integer -> Bool
isIncreasing n = and $ zipWith (<=) digits $ tail digits
	where digits = intToDigits n

isDecreasing :: Integer -> Bool
isDecreasing n = and $ zipWith (>=) digits $ tail digits
	where digits = intToDigits n

isBouncy :: Integer -> Bool
isBouncy n = not (isIncreasing n || isDecreasing n)

--------------------------------------------------------------------------------
-- Faster functions
--------------------------------------------------------------------------------

{-
Explanation

----------

To calculate the amount of n-digit decreasing numbers which contain d distinct
digits:

- There are (10 `choose` d) ways to choose which digits to use.
- There are ((n - 1) `choose` (d - 1)) ways to choose the arrangement of the
digits chosen. This is because you must change the current digit (d - 1)
times within (n - 1) spaces. e.g. for (n = 7) and (d = 4),

    a b b b c d d
      ^     ^ ^

The carets indicate where the current digit changed. There are 6 possible
places to put the 3 carets.

----------

To calculate the amount of n-digit increasing numbers which contain d distinct
digits, the reasoning is the same as above except:

- There are (9 `choose` d) ways to choose which digits to use, because
increasing numbers cannot contain 0.

----------

To calculate the amount of n-digit numbers that are not bouncy:

- Non-bouncy numbers are the union of increasing and decreasing numbers.
- Given a fixed number of digits, there are 9 digits which are both increasing
and decreasing.
- By the Inclusion-Exclusion Principle, the amount of n-digit non-bouncy numbers
equals the amount of n-digit decreasing numbers plus the amount of n-digit
increasing numbers minus 9 (the intersection).
-}

choose :: Integer -> Integer -> Integer
n `choose` k = product [n-k+1..n] `div` product [1..k]

-- Returns the amount of n-digit decreasing numbers.
numDecreasing :: Integer -> Integer
numDecreasing n = sum $ map (f n) [1..n]
	-- Returns the amount of n-digit decreasing numbers which contain d distinct digits.
	where f n d
		| d == 1 = 9
		| otherwise = ((n - 1) `choose` (d - 1)) * (10 `choose` d)

-- Returns the amount of n-digit increasing numbers.
numIncreasing :: Integer -> Integer
numIncreasing n = sum $ map (f n) [1..n]
	-- Returns the amount of n-digit increasing numbers which contain d distinct digits.
	where f n d
		| d == 1 = 9
		| otherwise = ((n - 1) `choose` (d - 1)) * (9 `choose` d)

-- Returns the amount of n-digit numbers that are not bouncy.
numNotBouncy :: Integer -> Integer
numNotBouncy n = numDecreasing n + numIncreasing n - 9

main :: IO ()
main = print $ sum $ map numNotBouncy [1..100]