{-
Because x, y, and z are in arithmetic progression, we can let
	x = a + 2d, y = a + d, z = a
for positive integers a and d.

Then
	n = (a+2d)^2 - (a+d)^2 - a^2
	n = -a^2 + 2ad + 3d^2
	n = c^2 - b^2
where
	b = a - d, c = 2d

To find the solutions (x, y, z), use the equation (c^2 = n + b^2).
Iterate through (b) values and find whether (n + b^2) is square.
If it is, then that (b) value gives a solution.

Given a fixed (b), the minimum (c) value is (c = b + 1).
Substituting, (n = c^2 - b^2 = (b + 1)^2 - b^2 = 2b + 1).
Solving for (b), (b = (n - 1) / 2).
So the maximum (b) value possible is ((n - 1) / 2).
It is only necessary to iterate for (n) upto that maximum value.

Note: (b) may be negative.
-}

import Time

sols :: Integer -> [(Integer, Integer)]
sols n = takeWhile ((> 0) . fst) [(a, d) |
	let lim = (n - 1) `div` 2,
	b <- [lim , lim - 1 .. -n], -- (-n) is arbitrary
	let (c, fPart) = properFraction $ sqrt $ fromIntegral $ n + b * b,
	fPart == 0,
	let
		d = c `div` 2
		a = b + d
		x = a + 2*d
		y = a + d
		z = a,
	x * x - y * y - z * z == n]

numSols :: Integer -> Int
numSols = length . sols

main :: IO ()
main = timePrint $ filter ((== 10) . numSols) [0..10^5]