-- 26 sec

import Data.Char (digitToInt)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

isIncreasing :: Integer -> Bool
isIncreasing n = and $ zipWith (<=) digits $ tail digits
	where digits = intToDigits n

isDecreasing :: Integer -> Bool
isDecreasing n = and $ zipWith (>=) digits $ tail digits
	where digits = intToDigits n

isBouncy :: Integer -> Bool
isBouncy n = not (isIncreasing n || isDecreasing n)

-- returns the first integer (n) such that the ratio of the bouncy numbers
-- in the range [1..n] to (n) is >= (ratio).
countBouncy :: Double -> Integer
countBouncy r = countBouncy' r 1 0
	where
	countBouncy' :: Double -> Integer -> Integer -> Integer
	countBouncy' ratio currentNum numBouncy
		| fromIntegral numBouncy / fromIntegral (currentNum - 1) >= ratio =
			currentNum - 1
		| otherwise = countBouncy' ratio (currentNum + 1) $
			if isBouncy currentNum then numBouncy + 1 else numBouncy

main :: IO ()
main = print $ countBouncy 0.99