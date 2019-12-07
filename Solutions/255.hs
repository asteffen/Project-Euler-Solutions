import Data.List (findIndex)
import Data.Maybe (fromJust)

roundedSqrt :: Integer -> Integer
roundedSqrt n = f x0
	where
		d = length $ show n
		x0 = if odd d
			then 2 * 10 ^ ((d - 1) `div` 2)
			else 7 * 10 ^ ((d - 2) `div` 2)
		f xk = if xk1 == xk then xk else f xk1
			where xk1 = (xk + ceiling (fromIntegral n / fromIntegral xk)) `div` 2

-- returns the number of iterations required when finding the rounded square
-- root of (n).
numIter :: Integer -> Integer
numIter n = (+1) $ fromIntegral $ fromJust $ findIndex id $ zipWith (==) xSeq $ tail xSeq
	where
		d = length $ show n
		x0 = if odd d
			then 2 * 10 ^ ((d - 1) `div` 2)
			else 7 * 10 ^ ((d - 2) `div` 2)
		f xk = (xk + ceiling (fromIntegral n / fromIntegral xk)) `div` 2
		xSeq = iterate f x0

-- average number of iterations required to find the rounded-square-root of a
-- (numDigits)-digit integer.
avgIter :: Integer -> Double
avgIter numDigits = (/ (fromIntegral $ 9 * min)) $ fromIntegral $ sum $
		map numIter [min .. 10 ^ numDigits - 1]
	where min = 10 ^ (numDigits - 1)