{-
u(k) is defined as the arithmetic-geometric sequence,

	u(k) = (900 - 3k) r ^ (k - 1), for k = 1, 2, ...

and s(n) is defined as the sum of the first n terms of u(k).

--------------------------------------------------------------------------------

let a(k) and g(k) be arithmetic and geometric sequences, respectively, such that

	u(k) = a(k) g(k)

from http://www.artofproblemsolving.com/Wiki/index.php/Arithmetico-geometric_series
, we have the sum of the first n terms of u(k) is:

	S(n) = a(n) g(n + 1) / (r - 1) - u(1) / (r - 1) - d (g(n + 1) - g(2)) / (r - 1) ^ 2

where d is the common difference of a(k) and r is the common ratio of g(k).
Note that in this formula, the series u(k) is also indexed starting at 1, so we
do not need to adjust.

To find the solution (r), we plug in the variables
	n = 5000
	S(n) = -600,000,000,000
	d = -3

and let q = r - 1.

	-600,000,000,000 = a(5000) g(5001) / q - u(1) / q - (-3) (g(5001) - g(2)) / q ^ 2
	-600,000,000,000 q ^ 2 = q a(5000) g(5001) - q u(1) + 3 (g(5001) - g(2))

a(5000) = 900 - 3 * 5000 = 900 - 15000 = -14100
g(5001) = r ^ (5001 - 1) = r ^ 5000
u(1) = (900 - 3 * 1) r ^ (1 - 1) = 897 r ^ 0 = 897
g(2) = r ^ (2 - 1) = r ^ 1 = r

	-600,000,000,000 q ^ 2 = q (-14100) (r ^ 5000) - q (897) + 3 (r ^ 5000 - r)
	-600,000,000,000 q ^ 2 = -14100 q r ^ 5000 - 897 q + 3 r ^ 5000 - r
	-600,000,000,000 (r ^ 2 - 2r + 1) = -14100 (r - 1) r ^ 5000 - 897 (r - 1) + 3 r ^ 5000 - r
	-600,000,000,000 r ^ 2 + 1,200,000,000,000 r - 600,000,000,000
		= -14100 r ^ 5001 + 14100 r ^ 5000 - 897 r + 897 + 3 r ^ 5000 - r
		= -14100 r ^ 5001 + (14100 r ^ 5000 + 3 r ^ 5000) + (-897 r - r) + 897
		= -14100 r ^ 5001 + 14103 r ^ 5000 - 898 r + 897
	-14100 r ^ 5001 + 14103 r ^ 5000 - 898 r + 897 +
		600,000,000,000 r ^ 2 - 1,200,000,000,000 r + 600,000,000,000 = 0
	-14100 r^5001 + 14103 r^5000 + 600,000,000,000 r^2 - 1,200,000,000,898 r + 600,000,000,897 = 0

I plugged this monstrosity into WolframAlpha and got r = 1.00232210876754.
http://www.wolframalpha.com/input/?i=-14100+r^5001+%2B+14103+r^5000+%2B+600%2C000%2C000%2C000+r^2+-+1%2C200%2C000%2C000%2C898+r+%2B+600%2C000%2C000%2C897+%3D+0

I tried putting 1.002322108767 and 1.002322108768 into the answer form, but neither worked.
So I experimented with the s function below and found that r = 1.002322108767 minimized
(s 5000 r + 6e11), out of all r with less than or equal to 12 digits after the decimal.
-}

a k = 900 - 3 * fromIntegral k
g r k = r ** (fromIntegral k - 1)
u r k = a k * g r k
s n r = sum $ map (u r) [1..n]