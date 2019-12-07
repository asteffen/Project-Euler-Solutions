{-
Consider an integer-sided triangle with side lengths (x, x+1, x+1).

The area of the triangle = sqrt(x^2(x+2)(3x+2)/16)

For the area to be an integer, x must be even and (x+2)(3x+2) must be square.

This is equivalent to the diophantine equation: 3x^2 - y^2 + 8x + 4 = 0.

From http://www.alpertron.com.ar/QUAD.HTM, the solutions to this diophantine
equation are given by

x(0) = 0
y(0) = -2

x(n+1) = -2x(n) - y(n) - 4
y(n+1) = -3x(n) - 2y(n) - 4

----------

Consider an integer-sided triangle with side lengths (x, x-1, x-1).

The area of the triangle = sqrt(x^2(x-2)(3x-2)/16)

For the area to be an integer, x must be even and (x-2)(3x-2) must be square.

This is equivalent to the diophantine equation: 3x^2 - y^2 - 8x + 4 = 0.

From http://www.alpertron.com.ar/QUAD.HTM, the solutions to this diophantine
equation are given by

x(0) = 0
y(0) = 2

x(n+1) = -2x(n) - y(n) + 4
y(n+1) = -3x(n) - 2y(n) + 4

----------

It turns out that the two (x, y)-sequences defined above are opposites of each
other. That is, each (x, y) in the first sequence corresponds to a (-x, -y) in
the second sequence.

In either case, the perimeter of the triangle = abs(3*x + 2).
-}

next :: (Integer, Integer) -> (Integer, Integer)
next (x, y) = (-2*x - y - 4, -3*x - 2*y - 4)

perimeter :: Integer -> Integer
perimeter x = abs $ 3*x + 2

solutions :: [Integer]
solutions = map fst $ drop 3 $ iterate next (0, -2)

main :: IO ()
main = print $ sum $ takeWhile (< 10^9) $ map perimeter solutions