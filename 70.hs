-- took about 6.25 hours

import Data.Function (on)
import Data.List (minimumBy, nub, sort)

-- primitive factor list, repeating primes. e.g. factorList 12 = [2,2,3].
factorList :: (Integral a) => a -> [a]
factorList 1 = []
factorList n = lpf : factorList (n `div` lpf)
	where lpf = leastPrimeFactor 2 n

leastPrimeFactor :: (Integral a) => a -> a -> a
leastPrimeFactor start n
	| start * start > n  = n
	| n `mod` start == 0 = start
	| otherwise          = leastPrimeFactor (start + 1) n

-- http://en.wikipedia.org/wiki/Totient_function#Computing_Euler.27s_function
totient :: Integer -> Integer
totient n = round $ fromIntegral n *
	product [1 - 1 / fromIntegral p | p <- nub $ factorList n]

--------------------------------------------------------------------------------
-- New functions
--------------------------------------------------------------------------------

-- returns (True) if 2 Integers are permutations of each other
arePerms :: Integer -> Integer -> Bool
arePerms = (==) `on` (sort . show)

-- returns (True) if (n) is a permutation of (totient n).
qualifies :: Integer -> Bool
qualifies n = arePerms n $ totient n

main = print $ fst $ minimumBy (compare `on` snd) $ map mapF $
	filter qualifies [2 .. 10^7]
	where mapF n = (n, fromIntegral n / fromIntegral (totient n))