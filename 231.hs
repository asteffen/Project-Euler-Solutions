-- for testing
binom :: Integer -> Integer -> Integer
binom n k = product [n - k + 1 .. n] `div` product [1 .. k]

-- the non-prime factors of the numerator and denominator used to calculate
-- he binomial coefficient.
numDenomFactors :: Integer -> Integer -> ([Integer], [Integer])
numDenomFactors n k = ([n - k + 1 .. n], [1 .. k])

-- reduces the fraction of ([numerator factors], [denominator factors]).
reduce :: ([Integer], [Integer]) -> ([Integer], [Integer])
reduce (xx@(x:xs), yy@(y:ys))
	| y == 1     = (xx, ys)
	| gcdxy /= 1 = ((x `div` gcdxy : xs), (y `div` gcdxy : ys))
	| otherwise  = consToFst x $ reduce (xs, yy)
	where
		gcdxy = gcd x y
		consToFst x (xs, ys) = (x : xs, ys)

fullyReduced :: Integer -> Integer -> [Integer]
fullyReduced n k = fst $ until (null . snd) reduce $ numDenomFactors n k