-- left this file a mess... was experimenting.

import Data.Char (digitToInt, intToDigit)
import Data.List (sort)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

sumOfDigits :: Integer -> Integer
sumOfDigits = fromIntegral . sum . intToDigits

factorial :: Integer -> Integer
factorial n = product [1..n]

f :: Integer -> Integer
f = sum . map (factorial . fromIntegral) . intToDigits

sf :: Integer -> Integer
sf = sumOfDigits . f

-- digit permutations such that
-- 1) each digit is >= the digit before it
-- 2) each contains no zeroes.
digitPerms :: Integer -> [[Integer]]
digitPerms 1 = map (map fromIntegral . intToDigits) [1..9]
digitPerms n = [current : rest | rest <- digitPerms (n - 1),
	current <- [1..head rest]]

-- this is equal to (map digitsToInt . digitPerms)
digitPerms2 :: Integer -> [Integer]
digitPerms2 1 = [1..9]
digitPerms2 n = [digitsToInt $ current : rest |
	rest <- map intToDigits $ digitPerms2 (n - 1),
	current <- [1 .. head rest]]

-- uses Ints
digitPerms3 :: Int -> [[Int]]
digitPerms3 1 = map (\e -> [e]) [1..9]
digitPerms3 n = [current : rest |
	rest <- digitPerms3 (n - 1),
	current <- [1.. head rest]]

-- in order
digitPerms4 :: Int -> [[Int]]
digitPerms4 1 = map (\e -> [e]) [1..9]
digitPerms4 n = sort [current : rest |
	rest <- digitPerms3 (n - 1),
	current <- [1.. head rest]]

-- digit permutations of all lengths (infinite list)
allDigitPerms :: [Integer]
allDigitPerms = concatMap digitPerms2 [1..]

-- same as above, but uses lists. e.g. [1,2] instead of 12.
allDigitPerms2 :: [[Int]]
allDigitPerms2 = concatMap digitPerms4 [1..]

g :: Integer -> Integer
g i = head [n | n <- [1..], sf n == i]

g2 :: Integer -> Integer
g2 i = head [n | n <- allDigitPerms, sf n == i]

-- factorial for ints
intFact :: Int -> Int
intFact n = product [1..n]

-- sum of digits for ints
intSumDig :: Int -> Int
intSumDig = sum . map digitToInt . show

-- more efficient that normal factorial
intFact2 :: Int -> Int
intFact2 1 = 1
intFact2 2 = 2
intFact2 3 = 6
intFact2 4 = 24
intFact2 5 = 120
intFact2 6 = 720
intFact2 7 = 5040
intFact2 8 = 40320
intFact2 9 = 362880

-- first integer to have a digital sum of n
minimalDS n = head [k | k <- [1..], sumOfDigits k == n]

g3 :: Int -> [Int]
g3 i = head [n | n <- allDigitPerms2, intSumDig (sum (map intFact n)) == i]

g4 :: Int -> [Int]
g4 i = head [n | n <- allDigitPerms2, intSumDig (sum (map intFact2 n)) == i]

sg :: Integer -> Integer
sg = sumOfDigits . g