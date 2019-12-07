import Data.List (sort)
import Data.Char (digitToInt, intToDigit)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

-- concatenated product of (d) and [1..n]
concatProd d n = concatMap (intToDigits . (*d)) [1..n]

isPandig = (== [1..9]) . sort

panProducts = [(d, n, digitsToInt cp) |
	d <- [1..10000],
	n <- [2..10],
	let cp = concatProd d n,
	isPandig cp]

third (_, _, x) = x

main = print $ maximum $ map third panProducts