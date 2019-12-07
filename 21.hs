-- this takes about 60 seconds to calculate.

proper_divisors :: Integer -> [Integer]
proper_divisors n = filter (\k -> n `mod` k == 0) [1..n `div` 2]

sum_proper_divisors :: Integer -> Integer
sum_proper_divisors = sum . proper_divisors

amicable_pairs :: [(Integer, Integer)]
amicable_pairs = [(a, b) | a <- [1..10000], let b = sum_proper_divisors a,
	a < b, a == sum_proper_divisors b]

main :: IO ()
main = print $ sum $ map (\(a, b) -> a + b) amicable_pairs