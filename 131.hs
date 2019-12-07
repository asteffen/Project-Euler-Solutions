{-
For (n^2 * (n + p)) to be a cube, (n) and (n + p) must be cubes.
This means that p is a difference of cubes.

The only way p can be prime is if the cubes are consecutive, because
	a^3 - b^3 = (a - b)(a^2 + ab + b^2)
if the cubes are consecutive, then (a - b = 1).

So (n) and (n + p) are consecutive cubes:
	n + p = (a + 1) ^ 3
	n = a^3
Solving for (p) yields
	p = 3a^2 + 3a + 1

Thus, the problem is reduced to finding all primes under 10^6 of the form
(3n^2 + 3n + 1).
-}

import Euler (isPrime)
import Time

-- 3a^2 + 3a + 1
g :: Integer -> Integer
g a = 3 * a * (a + 1) + 1

-- the number of primes under (lim) that have the property.
numQualify :: Integer -> Int
numQualify lim = length $ filter isPrime $ takeWhile (< lim) $ map g [1..]

main :: IO ()
main = timePrint $ numQualify $ 10^12