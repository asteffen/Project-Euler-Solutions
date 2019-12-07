-- 65 sec

import Data.List (nub)

factorList :: (Integral a) => a -> [a]
factorList 1 = []
factorList n = lpf : factorList (n `div` lpf)
	where lpf = leastPrimeFactor 2 n

leastPrimeFactor :: (Integral a) => a -> a -> a
leastPrimeFactor start n
	| start * start > n  = n
	| n `mod` start == 0 = start
	| otherwise          = leastPrimeFactor (start + 1) n

numDistinctFactors :: (Integral a) => a -> Int
numDistinctFactors = length . nub . factorList

-- returns the first (n) consecutive numbers to have (n) distinct prime factors
firstN :: Integer -> Integer
firstN n = head [k | k <- [1..],
	all ((==n) . fromIntegral . numDistinctFactors) [k..k+n-1]]

main :: IO ()
main = print $ firstN 4