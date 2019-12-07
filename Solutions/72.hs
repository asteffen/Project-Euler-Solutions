{-
I split the range [2..1000000] into smaller sub-ranges.
The syntax used to generate the table was:
	
	sum [totient n | n <- range]

           range |  output     | time (sec)
     [2..100000] |  3039650753 | 46.42
[100001..200000] |  9118948164 | 76.66
[200001..300000] | 15198149566 | 93.48
[300001..400000] | 21277458826 | 107.94
[400001..500000] | 27356832366 | 118.08
[500001..600000] | 33435937046 | 129.88
[600001..700000] | 39515212818 | 138.22
[700001..800000] | 45594757684 | 147.16
[800001..900000] | 51673635564 | 154.06
[900001..1000000]| 57752969604 | 159.59

total = 303963552391
(correct)
-}

import Data.List (nub, group)
import Data.Ratio ((%), numerator)

-- primitive factor list, repeating primes. e.g. factorList 12 = [2,2,3].
factorList :: (Integral a) => a -> [a]
factorList 1 = []
factorList n = lpf : factorList (n `div` lpf)
	where lpf = leastPrimeFactor 2 n

-- groups factors together. returns pairs of the form (prime, exponent).
groupedFactorList :: Integer -> [(Integer, Int)]
groupedFactorList = map (\xs -> (head xs, length xs)) . group . factorList

leastPrimeFactor :: (Integral a) => a -> a -> a
leastPrimeFactor start n
	| start * start > n  = n
	| n `mod` start == 0 = start
	| otherwise          = leastPrimeFactor (start + 1) n

{-
3 different totient functions: listed in order of speed, although they are all
pretty much about the same speed.

expression                    | time (sec)
sum $ map totient [1..60000]  | 22.05
sum $ map totient2 [1..60000] | 22.14
sum $ map totient3 [1..60000] | 22.58

(totient) uses the euler product with Doubles, then rounds to get the answer.
(totient2) uses the product above that on wikipedia.
(totient3) uses the same method as (totient), but with Rational instead of Double.
-}

-- http://en.wikipedia.org/wiki/Totient_function#Computing_Euler.27s_function
totient :: Integer -> Integer
totient n = round $ fromIntegral n *
	product [1 - 1 / fromIntegral p | p <- nub $ factorList n]

totient2 :: Integer -> Integer
totient2 n = product [(p - 1) * p ^ (e - 1) | (p, e) <- groupedFactorList n]

totient3 :: Integer -> Integer
totient3 n = numerator $ n % 1 *
	product [1 - 1 % fromIntegral p | p <- nub $ factorList n]

-- the number of reduced proper fractions (n / d) for d <= k
numFracs :: Integer -> Integer
numFracs k = sum [totient i | i <- [2..k]]