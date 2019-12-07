import Euler (ppt)

brute = [(x, y, z) |
	x <- [1..100],
	y <- [1 .. x-1],
	isSquare (x + y), isSquare (x - y),
	z <- [1 .. y-1],
	all isSquare [x + z, x - z, y + z, y - z]
	]

noz = [(x, y) |
	x <- [1..50],
	y <- [1 .. x-1],
	isSquare (x + y), isSquare (x - y)]

squares = map (\x -> x * x) [1..]

isSquare n = n `elem` takeWhile (<= n) squares