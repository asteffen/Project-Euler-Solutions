import Time

-- number of square laminae that can be formed using up to (n) tiles.
numSL :: Integer -> Int
numSL n = sum $ map (\x -> length $ takeWhile (<= q `div` x) [1..x-1]) [1..q]
	where q = n `div` 4

main = timePrint $ numSL $ 10^6