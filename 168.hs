import Time

rotr :: Integer -> Integer
rotr n = read $ l : i
	where
		s = show n
		i = init s
		l = last s

-- slower
rotr2 :: Integer -> Integer
rotr2 n = read $ show m ++ show d
	where (d, m) = n `divMod` 10

rotl :: Integer -> Integer
rotl n = read $ xs ++ [x]
	where (x:xs) = show n

qual :: Integer -> Bool
qual n = rotr2 n `mod` n == 0

-- doesn't work
qual2 :: Integer -> Bool
qual2 n = n `mod` rotl n == 0

-- faster
qual3 :: Integer -> Bool
qual3 n
	| l < h = False
	| otherwise = r `mod` n == 0
	where
		s = show n
		i = init s
		l = last s
		h = head s
		r = read $ l : i

main = timePrint $ sum $ filter qual3 [10..10^6]

{-
10^6 -> 98331
10^7 -> 98326
10^100 -> 97861

conjecture:
every time you increase the upper bound by 10x, the answer decreases by 5.

apparently it's incorrect
-}

-- r == (-5) `mod` (10^5)
r = sum $ map (11111 *) [1..9]

-- answer for 10^n
getAns n = 98331 - 5 * (n - 6)