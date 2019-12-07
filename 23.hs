{-
The actual limit is 20161. That is, every number greater than 20161 can be
expressed as a sum of two abundant numbers. from
http://mathworld.wolfram.com/AbundantNumber.html

Program doesn't finish when trying the entire range [1..28124].
So I split the range into smaller sub-ranges.
Note: In the following table, (abundant_nums) was pre-calculated up to 30000.

sub-range      |  output | run time (sec)
[1..5000]      | 2035227 |         19.91
[5001..10000]  | 1695777 |         57.73
[10001..15000] |  308935 |        105.23
[15001..20000] |  119771 |        155.81
[20001..25000] |   20161 |        201.06
[25001..28124] |       0 |        157.03

total = 2035227 + 1695777 + 308935 + 119771 + 20161 = 4179871
-}


-- returns the list of integers which divide (n), excluding itself.
proper_divisors :: Integer -> [Integer]
proper_divisors n = filter (\k -> n `mod` k == 0) [1..n `div` 2]

sum_proper_divisors :: Integer -> Integer
sum_proper_divisors = sum . proper_divisors

is_abundant :: Integer -> Bool
is_abundant n = sum_proper_divisors n > n

-- infinite list of abundant numbers
abundant_nums :: [Integer]
abundant_nums = filter is_abundant [1..]

-- returns list of all (a, b) such that
-- 1) a and b are abundant numbers
-- 2) a + b == n
sum_2_abundants :: Integer -> [(Integer, Integer)]
sum_2_abundants n = [(a, b) | a <- abundants_n, let b = n - a, b `elem` abundants_n]
	where
		-- abundant nubers less than n
		abundants_n :: [Integer]
		abundants_n = takeWhile (<n) abundant_nums

-- returns True if (n) cannot be written as the sum of 2 abundant numbers
not_sum_abundants :: Integer -> Bool
not_sum_abundants = null . sum_2_abundants

main :: IO ()
main = print $ sum $ filter not_sum_abundants [1..1000]