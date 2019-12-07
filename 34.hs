import Data.Char (digitToInt)

factorial :: Integer -> Integer
factorial n = product [1..n]

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: (Integral a, Num b) => a -> [b]
intToDigits = map (fromIntegral . digitToInt) . show

-- returns the sum of the factorials of the digits of (n)
sumDigitFac :: Integer -> Integer
sumDigitFac = sum . map factorial . intToDigits

-- returns (True) if (n == sumDigitFac n)
isCurious :: Integer -> Bool
isCurious n = n == sumDigitFac n

-- by experimentation, it seems 40585 is the highest curious number.
curiousNums :: [Integer]
curiousNums = filter isCurious [10..40585]

main :: IO ()
main = print $ sum curiousNums