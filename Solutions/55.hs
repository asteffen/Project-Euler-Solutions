-- 0.81 sec, faster than expected.

import Data.Char (digitToInt, intToDigit)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

reverseDigits :: Integer -> Integer
reverseDigits = digitsToInt . reverse . intToDigits

reverseAndAdd :: Integer -> Integer
reverseAndAdd n = n + reverseDigits n

isPalindrome :: Integer -> Bool
isPalindrome n = digits == reverse digits
	where digits = intToDigits n

-- note: the tail has to be taken, because if it isn't,
-- then it returns (False) for palindromic Lychrel numbers, such as 4994.
isLychrel :: Integer -> Bool
isLychrel n = not $ any isPalindrome $ take 50 $ tail $ iterate reverseAndAdd n

main :: IO ()
main = print $ length $ filter isLychrel [1..9999]