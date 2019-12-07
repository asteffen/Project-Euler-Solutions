{-
The period of the repeating decimal of a fraction (c / d)
is at most the smallest number n such that (10^n - 1) is divisible by d.
http://en.wikipedia.org/wiki/Repeating_decimal

I modified this statement to make a function which apparently returns the
exact period of a repeating decimal representation of the form (1 / n), where
(n) is a natural number.

I experimented with this function and it is true for many (n), but I have no
proof of its truth.

Call the function f. Then it is defined as follows:

f(d) =
	f(d / 2),                                if n is divisible by 2
	f(d / 5),                                if n is divisible by 5
	the lowest natrual number (n) such that
		(10^n - 1) is divisible by (d),      otherwise
-}

import Data.List (maximumBy)

-- returns (True) iff (n) is a factor of (k).
divides :: (Integral a) => a -> a -> Bool
n `divides` k = k `mod` n == 0

-- returns the first element in a list that satisfies predicate (p)
first_to_satisfy :: (a -> Bool) -> [a] -> a
first_to_satisfy p = head . filter p

-- This is the function which is defined by the block comment at the top of the file.
rep_dec_period d
	| d `mod` 2 == 0 = rep_dec_period $ d `div` 2
	| d `mod` 5 == 0 = rep_dec_period $ d `div` 5
	| otherwise      = first_to_satisfy (\n -> d `divides` (10^n - 1)) [1..]

{-
returns the infinite list of digits in the decimal representation of (1 / n).
the algorithm used is similar to long division.

Note: this is not needed to solve the problem, but is helpful for verifying
the output of (rep_dec_period).
-}
list_digits :: (Integral a) => a -> [a]
list_digits n = list_digits' n 10
	-- helper function: passes along the parameter d, which is the next dividend.
	where list_digits' n d = quotient : list_digits' n (10 * (d - n * quotient))
			where quotient = d `div` n

main :: IO ()
main = print $ fst $ maximumBy max_func $ map map_func [1..999]
	where
		max_func (_,period1) (_,period2) = period1 `compare` period2
		map_func n = (n, rep_dec_period n)