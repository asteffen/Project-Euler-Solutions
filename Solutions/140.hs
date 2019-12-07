-- related to problem 137

{-

ag(x)         = x + 4x2 + 5x3 + 9x4 + 14x5 + 23x6 + ...
x*ag(x)       =      x2 + 4x3 + 5x4 +  9x5 + 14x6 + ...
-------------------------------------------------------
ag(x)-x*ag(x) = x + 3x2 +  x3 + 4x4 +  5x5 +  9x6 + ...
              = x + 3x2 +x2(x + 4x2 +  5x3 +  9x4 + ...)
			  = x + 3x2 + x2*ag(x)
ag(x) - x*ag(x) - x2*ag(x) = x + 3x2
ag(x) * (1 - x - x2) = x + 3x2
ag(x) = (x + 3x2) / (1 - x - x2)

finding the inverse of ag:
x = (y + 3y2) / (1 - y - y2)
x - xy - xy2 = y + 3y2
3y2 + xy2 + y + xy - x = 0
(3 + x)y2 + (1 + x)y + (-x) = 0

y = -x - 1 +- sqrt ((x+1)^2 - 4(-x)(3+x))
    -------------------------------------
    2(x+3)

discriminant = x2 + 2x + 1 + 4x(x+3)
             = x2 + 2x + 1 + 4x2 + 12x
			 = 5x2 + 14x + 1

y = (sqrt (5x2 + 14x + 1) - x - 1) / (2(x+3))
-}

x1 = (sqrt 5 - 1) / 4
x2 = 2 / 5
x3 = (sqrt 22 - 2) / 6
x4 = (sqrt 137 - 5) / 14
x5 = 1 / 2

ag x = x * (1 + 3*x) / (1 - x * (1 + x))
agInv y = (sqrt (5*y*y + 14*y + 1) - y - 1) / (2 * (y + 3))

{-
we want to find the (y)s such that (5y2 + 14y + 1) is square.

Using http://www.alpertron.com.ar/QUAD.HTM,
I found the solutions of the diophantine equation (5y2 + 14y + 1 - x2 = 0)

(-1, 0) -> (37,-18)
( 1, 0) -> (19,-10)
(-7, 2) -> (131,-60)
( 7, 2) -> (5,-4)       x
(-2,-3) -> (-14,5)
( 2,-3) -> (-50,21)     x
(-5,-4) -> (-7,2)       x
( 5,-4) -> (-97,42)
(14, 5) -> (2,-3) -> (-50, 21)
-}

nextX, nextY :: Integer -> Integer -> Integer
nextX x y = -9*x + 20*y + 28
nextY x y = 4*x - 9*y - 14

nextPair :: (Integer, Integer) -> (Integer, Integer)
nextPair (x, y) = (nextX x y, nextY x y)

sols = map (iterate nextPair) [(-1,0),(1,0),(-7,2),(-2,-3),(5,-4),(14,5)]
ysols = map (filter (>0) . map snd) sols

-- all solutions in y. infinite list of golden nuggets
nuggets :: [Integer]
nuggets = f ysols
	where
		f ylists = y : f (map (delFstEq y) ylists)
			where (y:ys) = minimum ylists
		--f ylists = y : f (ys : filter ((/= y) . head) ylists)
		--where (y:ys) = minimum ylists
		delFstEq a (b:bs)
			| a == b = bs
			| otherwise = b:bs

main = print $ sum $ take 30 nuggets