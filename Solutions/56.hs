import Data.Char (digitToInt, intToDigit)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

digitalSum :: Integer -> Int
digitalSum = sum . intToDigits

main :: IO ()
main = print $ maximum [digitalSum (a ^ b) | a <- [1..100], b <- [1..100]]