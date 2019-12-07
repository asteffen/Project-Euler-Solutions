numDigits :: Integer -> Integer
numDigits = fromIntegral . length . show

-- returns the number of numbers that are an (n)th power and have (n) digits.
numQualified :: Integer -> Int
numQualified n = length $ filter ((== n) . snd) $ takeWhile ((<= n) . snd) $
	map mapFunc [1..]
	where mapFunc k = (nthPower, numDigits nthPower)
		where nthPower = k ^ n

-- by experimentation, numQualified returns 0 for every n > 21.
main :: IO ()
main = print $ sum $ map numQualified [1..21]