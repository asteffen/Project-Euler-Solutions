-- 2.41 sec

import Data.Char (digitToInt)

sum_fifth_powers :: Integer -> Integer
sum_fifth_powers = fromIntegral . sum . map ((^5) . digitToInt) . show

eq_fifth_powers :: Integer -> Bool
eq_fifth_powers n = n == sum_fifth_powers n

-- by experimentation, it seems 194979 is the highest number that can be
-- expressed as the sum of the fifth powers of its digits.
main :: IO ()
main = print $ sum $ filter eq_fifth_powers [2..200000]