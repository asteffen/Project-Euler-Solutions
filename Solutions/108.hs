{-
Answer: 180180
The running time is extremely long -- at least 12 hours.
-}

import Data.List (find)

-- Given (n), returns the solutions (x, y) to (1/x + 1/y == 1/n).
findSols :: Integer -> [(Integer, Integer)]
findSols n = [(x, y) | x <- [n + 1 .. 2 * n],
	let (y, m) = (n * x) `divMod` (x - n), m == 0]

check :: [Integer] -> IO ()
check xs = do
	putStrLn $ "Checking up to " ++ show (last current)
	case find ((> 1000) . length . findSols) current of
		Just x  -> putStrLn $ "Found it: " ++ show x
		Nothing -> check rest
	where (current, rest) = splitAt 1000 xs

main :: IO ()
main = check [129*1000..]