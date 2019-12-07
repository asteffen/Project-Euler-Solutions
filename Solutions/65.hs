import Data.Ratio ((%), numerator, denominator)
import Data.Char (digitToInt)

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- sum of the digits
sumOfDigits :: Integer -> Int
sumOfDigits = sum . intToDigits

-- infinite list of the continued fraction for e.
contFracE :: [Integer]
contFracE = 2 : concat [[1, k, 1] | k <- [2,4..]]

-- converts a continued fraction into a Rational.
fromContFrac :: [Integer] -> Rational
fromContFrac [x] = x % 1
fromContFrac (x:xs) = x % 1 + recip (fromContFrac xs)

-- returns the numerator and denominator of a Rational.
parts :: Rational -> (Integer, Integer)
parts x = (numerator x, denominator x)

-- infinite list of the convergents of e.
convergentsE :: [(Integer, Integer)]
convergentsE = [parts $ fromContFrac $ take n contFracE | n <- [1..]]

main :: IO ()
main = print $ sumOfDigits $ fst $ convergentsE !! 99