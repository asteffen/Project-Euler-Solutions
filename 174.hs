ltype n = length [(x, y) | x <- [1..q], let (y, m) = q `divMod` x, m == 0, y < x]
	where q = n `div` 4