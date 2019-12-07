-- 229.70 sec

import Data.Char (digitToInt)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

qualifies :: Integer -> Bool
qualifies n = case intToDigits n of
	[1, _, 2, _, 3, _, 4, _, 5, _, 6, _, 7, _, 8, _, 9, _, 0] -> True
	_ -> False

square :: Integer -> Integer
square n = n * n

{-
let (n) be the integer whose square qualifies.

sqrt 1020304050607080900 <= n <= sqrt 1929394959697989990
1010101010 <= n <= 1389026624

also, since (n^2) has a last digit of 0, (n) must have a last digit of 0.
-}

main :: IO ()
main = print $ head $ filter (qualifies . square) [1010101010,1010101020..1389026624]