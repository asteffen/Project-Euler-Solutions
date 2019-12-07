import Data.List (maximumBy)
import Data.Function (on)
import Time

modExp :: Integer -> Integer -> Integer -> Integer
modExp m x0 y0
	| y0 < 0    = error "Negative exponent"
	| y0 == 0   = 1
	| otherwise = f x0 y0
	where
		f x y
			| even y    = f (x * x `mod` m) (y `quot` 2)
			| y == 1    = x
			| otherwise = g (x * x `mod` m) ((y - 1) `quot` 2) x
		g x y z
			| even y    = g (x * x `mod` m) (y `quot` 2) z
			| y == 1    = x * z `mod` m
			| otherwise = g (x * x `mod` m) ((y - 1) `quot` 2) (x * z `mod` m)

getR :: Integer -> Integer -> Integer
getR a n = ((a - 1) ^ n + (a + 1) ^ n) `mod` (a * a)

-- apparently slower
getR2 :: Integer -> Integer -> Integer
getR2 a n = (modExp a2 (a - 1) n + modExp a2 (a + 1) n) `mod` a2
	where a2 = a * a

rMax :: Integer -> Integer
rMax a = maximum $ map (getR a) [1..2000]

main :: IO ()
main = timePrint $ sum $ map rMax [3..1000]