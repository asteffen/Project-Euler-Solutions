import Data.List (nub)

-- generates all "magic" 3-gon rings, for testing
magicTri :: [[[Int]]]
magicTri = [ [[a, b, c], [d, c, e], [f, e, b]] |
	a <- [1..6],
	b <- [1..6], b /= a,
	c <- [1..6], c /= a, c /= b,
	d <- [1..6], d /= a, d /= b, d /= c,
	e <- [1..6], e /= a, e /= b, e /= c, e /= d,
	a + b + c == d + c + e,
	f <- [1..6], f /= a, f /= b, f /= c, f /= d, f /= e,
	f + e + b == a + b + c]

-- rotates a list until its least element is at the front
leastFirst :: (Ord a) => [a] -> [a]
leastFirst list = leastFirst' list
	where
		m = minimum list
		leastFirst' xx@(x:xs)
			| x == m    = xx
			| otherwise = leastFirst' $ xs ++ [x]

{-
         a
		  \
		   b   d
	 	 -  \ /
	   i     c
     -  \   /
   j     g-e-f
          \
		   h
-}

-- generates all "magic" 5-gon, including different orders.
magicPent :: [[[Int]]]
magicPent = [ [[a, b, c], [d, c, e], [f, e, g], [h, g, i], [j, i, b]] |
	a <- xs,
	b <- xs, b `notElem` [a],
	c <- xs, c `notElem` [a, b],
	d <- xs, d `notElem` [a, b, c],
	e <- xs, e `notElem` [a, b, c, d],
	let s = a + b + c,
	d + c + e == s,
	f <- xs, f `notElem` [a, b, c, d, e],
	g <- xs, g `notElem` [a, b, c, d, e, f],
	f + e + g == s,
	h <- xs, h `notElem` [a, b, c, d, e, f, g],
	i <- xs, i `notElem` [a, b, c, d, e, f, g, h],
	h + g + i == s,
	j <- xs, j `notElem` [a, b, c, d, e, f, g, h, i],
	j + i + b == s]
	where xs = [1..10]

-- same as magicPent, but eliminates rotations.
-- it does this by starting from the group of three with the numerically
-- lowest external node, as described in the problem.
magicPent' :: [[Int]]
magicPent' = map concat $ nub $ map leastFirst magicPent

-- answer to the problem.
answer :: String
answer = concatMap show $ last magicPent'

main :: IO ()
main = putStrLn answer