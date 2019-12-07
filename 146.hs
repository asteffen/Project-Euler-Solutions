{-
Because (n^2 + 1) and (n^2 + 3) are twin primes, (n^2 + 2) is a multiple of 6.
	n^2 + 2 = 0 (mod 6)
	n^2 = 4     (mod 6)
	n = 2 or 4  (mod 6)
-}

import Data.Numbers.Primes (primes)
import Euler (isPrime)
import Time
import Data.List -- (findIndices, isPrefixOf)

qual :: Integer -> Bool
qual n = and $ zipWith (==) (map (+ n*n) [1, 3, 7, 9, 13, 27]) $ dropWhile (< n*n+1) primes

qual2 :: Integer -> Bool
qual2 n = map (\x -> isPrime $ n*n + x) [1,3..27] ==
	[True, True, False, True, True, False, True, False, False, False, False, False, False, True]
--   1     3     5      7     9     11     13    15     17     19     21     23     25     27

possibles :: [Integer]
possibles = concat [[n + 10, n + 20] | n <- [0, 30 ..]]

--main = timePrint $ filter qual2 $ takeWhile (< 10^6) possibles
main = timePrint $ take 3 listQuals


listQuals :: [Integer]
listQuals = [n |
	p <- p2424,
	let (n, fracPart) = properFraction $ sqrt $ fromIntegral p - 1,
	fracPart == 0]

---- for experimenting
{-
it seems that all elements p of p14:
	p = 5 (mod 6)

If this is true, then
because (n^2 + 13) and (n^2 + 27) are consecutive primes with a difference of 14,
	n^2 + 13 = 5 (mod 6)
	n^2 = 4      (mod 6)
	n = 2 or 4   (mod 6)

No new information is gained.

----

better idea:

search prime gaps for the sub-list [2, 4, 2, 4, 14]
and see if the primes have any notable congruences.

----

did the above method. findings:

if [p, p+2, p+6, p+8, p+12, p+26] is a list of consecutive primes, then
	p = 11 (mod 30)
	
	n^2 + 1 = 11 (mod 30)
	n^2 = 10     (mod 30)
	n = 10, 20   (mod 30)
-}

diffs xs = zipWith (-) (tail xs) xs

primeGaps = diffs primes

-- infinite list of (a) such that (a) and (a + 14) are consecutive primes
p14 = map (primes !!) $ findIndices (== 14) primeGaps

-- a list of indices where the sublist (sub) starts inside the larger list (xs).
sublistIndices :: (Eq a) => [a] -> [a] -> [Int]
sublistIndices = f 0
	where
		f :: (Eq a) => Int -> [a] -> [a] -> [Int]
		f i sub [] = []
		f i sub xs
			| sub `isPrefixOf` xs = i : rest
			| otherwise = rest
			where rest = f (i + 1) sub $ tail xs

is = sublistIndices [2, 4, 2, 4, 14] primeGaps

p2424 = map (primes !!) is

xx=[101,16061,201821,361211,465161,633461,857951,1063961,1091261,1210871,1351241,1368461,1954361,2136131,2225051,2342771,2418671,2499941,2508041,2564321,2728541,2764121,3103271,3436241,4042601,4336091,4740641,5436281,6187451,6900911,7161851,7166141,7464551,8166071,8540501,9083021,9146981,9508061,10676711,11021861,11664551,11904281,12451841,14520551,14834711,16353341,17339081,17863121,17946281,18601481]

h m = sort $ nub $ map (`mod` m) xx