import Data.Char (digitToInt)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

digitalSum :: Integer -> Integer
digitalSum = fromIntegral . sum . intToDigits

-- Returns floor (sqrt n).
intSqrt :: Integer -> Integer
intSqrt n = intSqrt' n 0 n
	where intSqrt' num min max
		| max == min + 1 = avg
		| otherwise      = case comparedToNum of
			EQ -> avg
			LT -> intSqrt' num avg max
			GT -> intSqrt' num min avg
			where
				avg = (min + max) `div` 2
				comparedToNum = (avg * avg) `compare` num

-- returns the sum of the first 100 decimal digits of (sqrt n).
-- I chose 10^210 because the sqrt of a number has roughly half its digits.
-- the sqrt of 10^210 has at least 100 digits (added 10 for safety).
sum100 :: Integer -> Integer
sum100 = fromIntegral . sum . take 100 . intToDigits . intSqrt . (*10^210)

-- returns (True) if the argument is not a perfect square
notSquare :: Integer -> Bool
notSquare = (/=0) . snd . properFraction . sqrt . fromIntegral

main = print $ sum [sum100 n | n <- [1..100], notSquare n]