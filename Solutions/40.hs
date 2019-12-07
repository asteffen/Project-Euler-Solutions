import Data.Char (digitToInt)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- the decimal digits of the irrational number 0.1234567891011121314...
-- 0 added to the front so that (num !! n) returns the (n)th digit
num = 0 : concatMap intToDigits [1..]

main = print $ product $ map (num !!) [1, 10, 100, 1000, 10000, 100000, 1000000]