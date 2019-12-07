main :: IO ()
main = putStrLn $ show $ sum $ filter isMult3or5 [1..999]
	where
		isMult3or5 :: Int -> Bool
		isMult3or5 n = n `mod` 3 == 0 || n `mod` 5 == 0