{-
Calling main with the range [1..1000000] caused a stack overflow error.
So I split up the range and called main with the smaller ranges.

            range | output | nDivTotN output   | time (sec)
      [1..100000] |  90090 | 5.213541666666667 |      64.28
 [100000..200000] | 180180 | 5.213541666666667 |     101.75
 [200000..300000] | 270270 | 5.213541666666667 |     123.06
 [300000..400000] | 390390 | 5.213541666666667 |     139.69
 [400000..500000] | 480480 | 5.213541666666667 |     153.09
 [500000..600000] | 510510 | 5.539388020833333 |     166.22
 [600000..700000] | 690690 | 5.450520833333333 |     178.00
 [700000..800000] | 746130 | 5.397352430555555 |     188.69
 [800000..900000] | 870870 | 5.399739583333333 |     197.36
[900000..1000000] | 930930 | 5.387326388888889 |     207.75

answer = 510510

Note: This makes sense because 510510 is 17#, where # means primorial.
http://en.wikipedia.org/wiki/Primorial
-}

import Data.List (maximumBy, nub)
import Data.Function (on)

factorList :: (Integral a) => a -> [a]
factorList 1 = []
factorList n = lpf : factorList (n `div` lpf)
	where lpf = leastPrimeFactor 2 n

leastPrimeFactor :: (Integral a) => a -> a -> a
leastPrimeFactor start n
	| start * start > n  = n
	| n `mod` start == 0 = start
	| otherwise          = leastPrimeFactor (start + 1) n

-- first version of totient. Much slower, but easier to understand.
totient_old :: (Integral a) => a -> Int
totient_old n = length $ filter ((==1) . gcd n) [1..n]

-- http://en.wikipedia.org/wiki/Totient_function#Computing_Euler.27s_function
totient :: (Integral a) => a -> Int
totient n = round $ fromIntegral n *
	product [1 - 1 / fromIntegral p | p <- nub $ factorList n]

-- n / totient(n)
nDivTotN :: (Integral a) => a -> Double
nDivTotN n = fromIntegral n / fromIntegral (totient n)

main :: IO ()
main = print $ fst $ maximumBy (compare `on` snd) $
	map (\n -> (n, nDivTotN n)) [10,20..1000000]