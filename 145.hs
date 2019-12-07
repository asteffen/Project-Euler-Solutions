{-
Was able to brute force this problem in C++, so I coded it in C++ instead.

project name = "pe145"
-}

import Data.Char (digitToInt, intToDigit)
import Time

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
--intToDigits :: Integer -> [Int]
intToDigits :: Int -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
--digitsToInt :: [Int] -> Integer
digitsToInt :: [Int] -> Int
digitsToInt = read . map intToDigit

isReversible n = all odd $ intToDigits $ (n+) $ digitsToInt $ reverse $ intToDigits n

reversibles = [n | n <- [1..10^9], last (intToDigits n) /= 0, isReversible n]

numRev n = length $ filter qualifies [1..n]

qualifies k = last (intToDigits k) /= 0 && isReversible k

-- slightly faster
qualifies2 n
	| ld == 0 = False
	| even $ head digits + ld = False
	| otherwise = isReversible n
	where
		digits = intToDigits n
		ld = last digits

main = timePrint $ numRev $ 10^6