-- 42 sec

import Data.Numbers.Primes (primes)
--import Time

{-
infinite 2-dimensional list of distinct semiprimes.
4 6 10 14 22
  9 15 21 33
    25 35 55
	   49 77
	      121
-}
spGrid :: [[Integer]]
spGrid = [map (* p) $ drop i primes | i <- [0..], let p = primes !! i]

-- semiprimes below n
spsBelow :: Integer -> [Integer]
spsBelow n = concat $ takeWhile (not . null) $ map (takeWhile (< n)) spGrid

main = timePrint $ length $ spsBelow $ 10^8