pythagList :: [(Int,Int,Int)]
pythagList = [ (a, b, c_int) | a <- [1..1000], b <- [a..1000],
	let
		c              = sqrt (fromIntegral (a*a + b*b))
		(c_int,c_frac) = properFraction c,
	c_frac == 0
	]

ans :: (Int,Int,Int)
ans = head $ filter (\(a,b,c) -> a + b + c == 1000) pythagList

main :: IO ()
main = putStrLn $ show $ (\(a,b,c) -> a * b * c) ans