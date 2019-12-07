{-

Let b = the number of blue discs
    n = the total number of discs

If the probability of taking 2 blue discs (without replacement) is 1, then

	b/n * (b-1)/(n-1) = 1/2

Solving for b, we get:

	b = (1 +- sqrt(2*n^2 - 2*n + 1)) / 2

If we take the minus sign, then b is only positive if 0 < n < 1. So it is safe
to always take the plus sign:

	b = (1 + sqrt(2*n^2 - 2*n + 1)) / 2

--------------------------------------------------------------------------------

In the same manner as above, we can derive the formula for n in terms of b:

	n = (1 + sqrt(8*b^2 - 8*b + 1)) / 2

--------------------------------------------------------------------------------

Note: because (2*n^2 - 2*n + 1 > 0) for all real n, it is safe to take the
sqrt of it.

--------------------------------------------------------------------------------

After finding some of the lower (n) such that (canBeN n == True), I plugged them
into OEIS, and found:

http://www.research.att.com/~njas/sequences/A046090

From there, I found that 1070379110497 is the lowest (n) greater than 10^12 such
that (canBeN n == True).

(numBlueDiscs2 1070379110497) = Just 756872327473,
so the answer is 756872327473.

-}

isInteger :: Double -> Bool
isInteger = (==0) . snd . properFraction

numBlueDiscs :: Integer -> Double
numBlueDiscs n' = (1 + sqrt (2*n*n - 2*n + 1)) / 2
	where n = fromIntegral n'

numBlueDiscs2 :: Integer -> Maybe Integer
numBlueDiscs2 n = case intSqrt (2*n*n - 2*n + 1) of
	Just k ->
		if (1 + k) `mod` 2 == 0
			then Just $ (1 + k) `div` 2
			else Nothing
	Nothing -> Nothing
			

-- returns if (n) can be the amount of discs, so that it is possible that
-- the probability of picking two discs is 1/2
canBeN :: Integer -> Bool
canBeN n = case intSqrt (2*n*n - 2*n + 1) of
	Just k  -> (1 + k) `mod` 2 == 0
	Nothing -> False

-- Integer square root: if n is a perfect square,
-- then return (Just sqrt(n))
-- else return (Nothing)
intSqrt :: Integer -> Maybe Integer
intSqrt n = intSqrt' n 0 n
	where intSqrt' num min max
		| max == min + 1 = Nothing
		| otherwise      = case comparedToNum of
			EQ -> Just avg
			LT -> intSqrt' num avg max
			GT -> intSqrt' num min avg
			where
				avg = (min + max) `div` 2
				comparedToNum = (avg * avg) `compare` num