import Data.List ((\\), nub, delete, genericLength)
import Data.Char (digitToInt)

-- [1..9] is used a lot, so I bound it to a variable.
to9 :: [Integer]
to9 = [1..9]

-- Every (n) digit number that satisfies:
-- digits are in (xs), and are distinct
digitsNoRep :: Integer -> [Integer] -> [Integer]
digitsNoRep 1 xs = xs
digitsNoRep n xs = [concatNumbers firstDigit remainingDigits | firstDigit <- xs,
	remainingDigits <- digitsNoRep (n-1) (delete firstDigit xs)]
	where concatNumbers first remaining = read (show first ++ show remaining)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: (Integral a) => a -> [Integer]
intToDigits = map (fromIntegral . digitToInt) . show

-- returns (True) if the list has distinct elements (every element is unique).
distinctElems :: (Eq a) => [a] -> Bool
distinctElems [] = True
distinctElems (x:xs) = all (/= x) xs && distinctElems xs

{-
let a, b = the multiplicands
    prod = a * b
	digits(n) = the number of digits in the decimal representation of n

We only need to test for the a, b such that
	digits(a) + digits(b) + digits(prod) is possibly equal to 9.

if digits(a) = 2 and digits(b) = 2, then 3 <= digits(prod) <= 4.
	2 + 2 + 3 = 7
	2 + 2 + 4 = 8
	either way, there are too few digits.

if digits(a) = 2 and digits(b) = 3, then 4 <= digits(prod) <= 5.
	2 + 3 + 4 = 9
	2 + 3 + 5 = 10
	so the sum of their digits is possibly 9.
	
if digits(a) = 3 and digits(b) = 3, then 5 <= digits(prod) <= 6.
	3 + 3 + 5 = 11
	3 + 3 + 6 = 12
	either way, there are too many digits.

if digits(a) = 1 and digits(b) = 4, then 4 <= digits(prod) <= 5
	1 + 4 + 4 = 9
	1 + 4 + 5 = 10
	so the sum of their digits is possibly 9.

Apparently, it is only necessary to test for the a, b such that
a is 2 digits and b is 3 digits, or
a is 1 digit  and b is 4 digits
-}

products23, products14 :: [(Integer, Integer, Integer)]
products23 = [(a, b, prod) |
	a <- digitsNoRep 2 to9,
	b <- digitsNoRep 3 $ to9 \\ intToDigits a,
	let
		prod = a * b
		prodDigits = intToDigits prod
		remainingDigits = (to9 \\ intToDigits a) \\ intToDigits b,
	distinctElems prodDigits,
	all (`elem` remainingDigits) prodDigits]

products14 = [(a, b, prod) |
	a <- digitsNoRep 1 to9,
	b <- digitsNoRep 4 $ to9 \\ intToDigits a,
	let
		prod = a * b
		prodDigits = intToDigits prod
		remainingDigits = (to9 \\ intToDigits a) \\ intToDigits b,
	distinctElems prodDigits,
	all (`elem` remainingDigits) prodDigits]

third :: (a, b, c) -> c
third (_, _, x) = x

main :: IO ()
main = print $ sum $ nub $ map third $ products23 ++ products14