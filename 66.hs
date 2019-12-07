{-
I used lots of info from:

http://en.wikipedia.org/wiki/Pell%27s_equations
http://en.wikipedia.org/wiki/Continued_fraction

Program runs in about 41 seconds.
-}

import Data.Ratio ((%), numerator, denominator)
import Data.List (inits, maximumBy)
import Data.Function (on)

-- converts a Rational to a continued fraction, expressed as a list of Integers.
toContFrac :: Rational -> [Integer]
toContFrac x
	| x == rat_floor_x = [floor_x]
	| otherwise        = floor_x : toContFrac next
	where
		floor_x = floor x
		rat_floor_x = floor_x % 1
		next = recip $ x - rat_floor_x

-- converts a continued fraction into a Rational.
fromContFrac :: [Integer] -> Rational
fromContFrac [x] = x % 1
fromContFrac (x:xs) = x % 1 + recip (fromContFrac xs)

-- returns the numerator and denominator of a Rational.
parts :: Rational -> (Integer, Integer)
parts x = (numerator x, denominator x)

-- returns a Rational that is within (precision) of (sqrt x).
rationalSqrt :: Rational -> Rational
rationalSqrt x = rationalSqrt' x 0 x
	where
		rationalSqrt' :: Rational -> Rational -> Rational -> Rational
		rationalSqrt' num min max
			| abs diff < precision = avg
			| diff > 0             = rationalSqrt' num min avg -- avg is too large
			| otherwise            = rationalSqrt' num avg max -- avg is too small
			where
				avg = (min + max) / 2
				diff = avg * avg - num
				
				-- I found 10^71 by testing if solvePell 661 gave an error.
				-- 10^71 is the smallest possible (of the form 10^n) that doesn't
				-- give an error.
				precision = 1 % 10^71

-- returns the convergents of the continued fraction of (sqrt n)
-- as pairs of (numerator, denominator)
-- Note: this version doesn't work because the precision of Doubles isnt hight enough.
convergents :: Integer -> [(Integer, Integer)]
convergents = map (parts . fromContFrac) . tail . inits .
	toContFrac . toRational . sqrt . fromIntegral

-- better than above, adjustable precision
convergents2 :: Integer -> [(Integer, Integer)]
convergents2 = map (parts . fromContFrac) . tail . inits .
	toContFrac . rationalSqrt . toRational

-- returns the fundamental (i.e. minimal) solution of the Diophantine equation
-- (x^2 - d*y^2 = 1) as an (x, y) pair.
solvePell :: Integer -> (Integer, Integer)
solvePell d = head $ filter isSolution $ convergents2 d
	where isSolution (x, y) = x*x - d*y*y == 1

-- returns x in the minimal solution of the Diophantine equation above.
solvePellX :: Integer -> Integer
solvePellX = fst . solvePell

-- non-squares in the range [1..1000].
nonSquares :: [Integer]
nonSquares = filter (not . isSquare) [1..1000]
	where isSquare = (==0) . snd . properFraction . sqrt . fromIntegral

main :: IO ()
main = print $ fst $ maximumBy (compare `on` snd) $
	map (\n -> (n, solvePellX n)) nonSquares