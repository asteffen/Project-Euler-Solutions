{-
iterated (sum of digits) is the same thing as (`mod` 9).

So in order for (n) to qualify,
	n `mod` 9 == (137 * n) `mod` 9
	n `mod` 9 == (2 * n) `mod` 9

This is only true when
	n `mod` 9 == 0
-}

import Data.Char (digitToInt)

sumDigits :: Integer -> Int
sumDigits = sum . map digitToInt . show

qualifies n = sumDigits n == sumDigits (137 * n)