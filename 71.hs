import Data.Ratio ((%), numerator)

-- returns the highest fraction that:
-- 1) has denominator <= k
-- 2) is less than (3 / 7)
-- note: since I optimized the ranges for d and n, it only works on high numbers.
highestFrac :: Integer -> Rational
highestFrac k = maximum [frac |
	d <- [k - 100 .. k],
	n <- [d * 3 `div` 7..d * 30001 `div` 70000],
	let frac = n % d,
	frac < 3 % 7]

main :: IO ()
main = print $ numerator $ highestFrac 1000000