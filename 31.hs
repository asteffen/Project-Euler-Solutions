-- returns the ways to express (n) as the sum of any amount of the elements in (xs).
-- putting the list in descending order makes it run faster.
ways_to_make :: (Integral a) => a -> [a] -> a
ways_to_make n (x:xs)
	| xs == []  = if n `mod` x == 0 then 1 else 0
	| otherwise = sum $ map (\r -> ways_to_make r xs) remainders
	where remainders = takeWhile (>=0) $ iterate (subtract x) n

main :: IO ()
main = print $ ways_to_make 200 [200, 100, 50, 20, 10, 5, 2, 1]