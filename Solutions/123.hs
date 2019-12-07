-- interpreted: about 4 sec.

import Data.Numbers.Primes (primes)
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

zippedPrimes :: [(Integer, Integer)]
zippedPrimes = zip [1..] primes

getRem :: (Integer, Integer) -> Integer
getRem (n, pn) = (modExp pn2 (pn - 1) n + modExp pn2 (pn + 1) n) `mod` pn2
	where pn2 = pn * pn

rems :: [(Integer, Integer)]
rems = map (\pair@(n, _) -> (n, getRem pair)) zippedPrimes

main :: IO ()
main = timePrint $ head $ filter ((>= 10^10) . snd) rems