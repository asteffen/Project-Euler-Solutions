{-
Again, I split up the range [1..9999999] into smaller sub-ranges.

Input:
length $ filter arrivesAt89 range

range              | output | time (sec)
      [1..1000000] | 856929 | 53.95
[1000001..2000000] | 861066 | 56.16
[2000001..3000000] | 858145 | 56.33
[3000001..4000000] | 859857 | 56.77
[4000001..5000000] | 859847 | 56.55
[5000001..6000000] | 860954 | 57.59
[6000001..7000000] | 858334 | 57.31
[7000001..8000000] | 858844 | 57.27
[8000001..9000000] | 852534 | 57.76
[9000001..9999999] | 854636 | 57.86

total = 8581146
-}

import Data.Char (digitToInt, intToDigit)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

next :: Integer -> Integer
next = fromIntegral . sum . map (^2) . intToDigits

arrivesAt89 :: Integer -> Bool
arrivesAt89 n = case n of
	1  -> False
	89 -> True
	_  -> arrivesAt89 $ next n

main :: IO ()
main = print $ length $ filter arrivesAt89 [4000001..5000000]