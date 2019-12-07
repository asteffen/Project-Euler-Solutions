-- 3.92 sec

import Data.Char (digitToInt)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- returns a list of the binary digits in a number
binaryDigits :: Integer -> [Int]
binaryDigits 0 = [0]
binaryDigits 1 = [1]
binaryDigits n = binaryDigits d ++ [fromIntegral m]
	where (d, m) = n `divMod` 2

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

palinBothBases :: Integer -> Bool
palinBothBases n = isPalindrome (intToDigits n) && isPalindrome (binaryDigits n)

main :: IO ()
main = print $ sum $ filter palinBothBases [1..1000000]