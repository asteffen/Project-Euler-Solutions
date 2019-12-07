-- 172.64 sec

import Data.Ratio ((%), numerator, denominator)

numDigits :: Integer -> Int
numDigits = length . show

-- infinite list of the continued fraction for (sqrt 2).
contFrac2 :: [Integer]
contFrac2 = 1 : repeat 2

-- converts a continued fraction into a Rational.
fromContFrac :: [Integer] -> Rational
fromContFrac [x] = x % 1
fromContFrac (x:xs) = x % 1 + recip (fromContFrac xs)

-- returns the numerator and denominator of a Rational.
parts :: Rational -> (Integer, Integer)
parts x = (numerator x, denominator x)

-- returns (True) if (num) has more digits than (denom).
numHasMoreDigits :: (Integer, Integer) -> Bool
numHasMoreDigits (num, denom) = numDigits num > numDigits denom

-- infinite list of the convergents of (sqrt 2).
convergents2 :: [(Integer, Integer)]
convergents2 = [parts $ fromContFrac $ take n contFrac2 | n <- [1..]]

main :: IO ()
main = print $ length $ filter numHasMoreDigits $ take 1000 convergents2