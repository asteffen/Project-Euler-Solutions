import Data.Ratio

prodPow xs = product $ zipWith (^) xs [1..]

sol n = map (\i -> i * n % k) [1..n]
	where k = sum [1..n]

maximize = prodPow . sol

ans = sum $ map (floor . fromRational . maximize) [2..15]

{-
P_2

x1 + x2 = 2
x2 = 2 - x1

P_2 = x_1 * (2 - x1)^2
P_2 = 1.18519 (approx) when x1 = 2/3
P_2 = 32/27

sol = (2/3, 4/3)

-----

P_3
x1 + x2 + x3 = 3
x1 = 3 - x2 - x3

P_3 = (3 - x2 - x3) * x2^2 * x3^3
P_3 = 27/16

sol = (1/2, 1, 3/2)

------
p_4

large fraction

sol = (2/5, 4/5, 6/5, 8/5)

----

note that all the solutions are of the form

(b, 2*b, 3*b, ...)
-}