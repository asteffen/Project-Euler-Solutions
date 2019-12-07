-- ~ 314 sec

import Data.Char (digitToInt, intToDigit)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: (Integral a) => a -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: (Integral a, Read a) => [Int] -> a
digitsToInt = read . map intToDigit

factorial :: Int -> Integer
-- factorial n = fromIntegral $ product [1..n]
factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial 3 = 6
factorial 4 = 24
factorial 5 = 120
factorial 6 = 720
factorial 7 = 5040
factorial 8 = 40320
factorial 9 = 362880

next :: [Int] -> [Int]
next = intToDigits . sum . map factorial

-- the non-repeating chain starting with (n).
-- expressed as a list of lists of digits in reverse order.
nonRep :: Integer -> [[Int]]
nonRep n = nonRep' [] $ intToDigits n
	where
		nonRep' :: [[Int]] -> [Int] -> [[Int]]
		nonRep' acc digits
			| digits `elem` acc = acc
			| otherwise = nonRep' (digits : acc) $ next digits

sixtyNonReps :: [Integer]
sixtyNonReps = filter ((==60) . length . nonRep) [1..1000000]

main :: IO ()
main = print $ length $ sixtyNonReps