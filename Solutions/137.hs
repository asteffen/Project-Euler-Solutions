{-
I found these using algebra, by first calculating
af(x)-x*af(x)
-}
af, afInv :: Double -> Double
af x = x / (1 - x - x * x)
afInv y = (sqrt (5*y*y + 2*y + 1) - y - 1) / (2 * y)

x1 = sqrt 2 - 1
x2 = 1 / 2
x3 = (sqrt 13 - 2) / 3
x4 = (sqrt 89 - 5) / 8
x5 = (sqrt 34 - 3) / 5

{-
I found the recursion using
http://www.alpertron.com.ar/QUAD.HTM

initial solutions: (-1, 0), (1, 0), (-2, -1), (2, -1)

(1, 0) -> (-5, 2)
(-1, 0) -> (13, -6)
(-2, -1) -> (2, -1) -> (-34, 15)
-}

nextX, nextY :: Integer -> Integer -> Integer
nextX x y = -9*x + 20*y + 4
nextY x y = 4*x - 9*y - 2

nextPair :: (Integer, Integer) -> (Integer, Integer)
nextPair (x, y) = (nextX x y, nextY x y)

sols1, sols2, sols3 :: [(Integer, Integer)]
sols1 = (1, 0) : map nextPair sols1
sols2 = (-1, 0) : map nextPair sols2
sols3 = (-2, -1) : map nextPair sols3

y1, y2, y3 :: [Integer]
y1 = filter (>1) $ map snd sols1
y2 = filter (>1) $ map snd sols2
y3 = filter (>1) $ map snd sols3

-- all solutions in y. infinite list of golden nuggets
nuggets :: [Integer]
nuggets = f [y1, y2, y3]
	where f ylists = y : f (ys : filter ((/= y) . head) ylists)
		where (y:ys) = minimum ylists