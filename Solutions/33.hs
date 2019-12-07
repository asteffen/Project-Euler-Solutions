import Data.Char (digitToInt)
import Data.Ratio

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: (Integral a, Num b) => a -> [b]
intToDigits = map (fromIntegral . digitToInt) . show

-- if 2 2-element lists share an element, return the other (non-equal) elements.
-- else return (0,0) as a "silent error" which will be checked in canCancelDigits.
cancel :: [Integer] -> [Integer] -> (Integer, Integer)
cancel [x1,x2] [y1,y2] =
	if x1 == y1
		then (x2,y2)
		else
			if x1 == y2
				then (x2,y1)
				else
					if x2 == y1
						then (x1,y2)
						else
							if x2 == y2
								then (x1,y1)
								else (0,0)

-- returns (True) if you can incorrectly "cancel the digits" in the fraction,
-- yet get a correct answer.
-- Note: (num * d == denom * n) is equivalent to (num / denom = n / d)
canCancelDigits :: Integer -> Integer -> Bool
canCancelDigits num denom = num * d == denom * n
	&& 0 `notElem` num_digits
	&& 0 `notElem` denom_digits
	&& n /= 0
	&& d /= 0
	where
		num_digits = intToDigits num
		denom_digits = intToDigits denom
		(n,d) = cancel num_digits denom_digits

-- all fractions where:
-- 1) you can "cancel the digits" and get a correct answer.
-- 2) the numerator and denominator each have 2 digits.
-- 3) the fraction is less than 1.
cancelFracs :: [(Integer, Integer)]
cancelFracs = filter (uncurry canCancelDigits) twoDigitFracs
	where twoDigitFracs = [(n,d) | n <- [10..99], d <- [n+1..99]]

main :: IO ()
main = print . round . fromRational . recip . product . map (uncurry (%)) $ cancelFracs