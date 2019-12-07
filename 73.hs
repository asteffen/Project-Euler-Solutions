-- ~19 sec

-- given a denominator (d), returns the number of reduced fractions n/d
-- such that 1/3 < n/d < 1/2.
numeratorRange :: Int -> Int
numeratorRange d = length $ filter ((==1) . gcd d) [minN..maxN]
	where
		-- the maximum and minimum possible values for (n). (inclusive)
		minN = d `div` 3 + 1
		maxN = if even d
			then d `div` 2 - 1
			else d `div` 2

main :: IO ()
main = print $ sum $ map numeratorRange [1..12000]