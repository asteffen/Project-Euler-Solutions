import Data.List (genericIndex)

-- the number of zeros n! ends in.
numZeros n = sum [n `div` pwr | pwr <- powersOf5]
	where powersOf5 = takeWhile (<= n) $ iterate (*5) 5

-- naive method used to check above.
numZeros2 :: Integer -> Int
numZeros2 = length . takeWhile (== '0') . reverse . show

modMult :: Integer -> Integer -> Integer -> Integer
modMult m a b = a * b `mod` m

factorial :: Integer -> Integer
factorial n = product [1..n]

-- last 5 nonzero digits of n!
-- for checking
last5 :: Integer -> Integer
last5 = read . reverse . take 5 . dropWhile (== '0') . reverse . show . factorial

trimTrailingZeros :: Integer -> Integer
trimTrailingZeros n
	| mod10 == 0 = trimTrailingZeros div10
	| otherwise  = n
	where (div10, mod10) = n `divMod` 10

last5list :: [Integer]
last5list = 1 : [trimTrailingZeros next `mod` 10^5 | n <- [0..],
	let next = (n + 1) * genericIndex last5list n]