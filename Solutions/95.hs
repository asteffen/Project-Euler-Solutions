{-
I actually didn't find the answer with this program, I found it here:
http://mathworld.wolfram.com/SociableNumbers.html

----------

1-chains (perfect numbers)
6 28 496 8128

2-chains (amicable pairs)
(0, 1)? (220, 284) (1184, 1210) (2620, 2924) (5520, 5564) (6232, 6368)
(10744, 10856) (12285, 14595) (17296, 18416)

5-chains
(12496, 14288, 15472, 14536, 14264)

28-chains
(14316,19116,31704,47616,83328,177792,295488,629072,589786,294896,358336,418904,366556,274924,275444,
243760,376736,381028,285778,152990,122410,97946,48976,45946,22976,22744,19916,17716)
-}

import Euler (factorTD)
import Data.List (group, genericIndex)
import Time

-- A list of the proper divisors of the given Integer.
-- The proper divisors of a number are all the divisors excluding the number itself.
divisors :: Integer -> [Integer]
divisors = init . f . map (\xs -> (head xs, length xs)) . group . factorTD
	where
		f [] = [1]
		f ((p, e) : pairs) = [x * y | x <- take (e + 1) $ iterate (* p) 1, y <- f pairs]

-- The sum of the proper divisors of the given Integer.
sumDivisors :: Integer -> Integer
sumDivisors = sum . divisors

-- The same thing as (sumDivisors), but memoized using (cache).
-- Does not work on values > 10^6; returns 1 instead.
sumDivMemoized :: Integer -> Integer
sumDivMemoized n
	| n <= 1000000 = genericIndex cache n
	| otherwise = 1

cache :: [Integer]
cache = map sumDivisors [0..10^6]

{-
Given (f) and (x), returns (lambda, mu) where
lambda = the cycle length of [x, f x, f (f x), ...]
mu = the index where the cycle starts

http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare
-}
floyd :: (Eq a) => (a -> a) -> a -> (Int, Int)
floyd f x = (lambda, mu)
	where
		xs = iterate f x
		
		-- Possibly a multiple of the real lambda. (lambda = cycle length)
		lambda0 = head $ filter (\i -> xs !! i == xs !! (2 * i)) [1..]
		
		-- Index where the repeating portion starts.
		mu = head $ filter (\i -> xs !! i == xs !! (i + lambda0)) [0..]
		
		-- The actual cycle length.
		lambda = head $ filter (\i -> xs !! mu == xs !! (mu + i)) [1..]

-- A list of all numbers which are part of an amicable chain
numsInChain :: [Integer]
numsInChain = filter ((== 0) . snd . floyd sumDivMemoized) [0..]

main = mapM_ print numsInChain