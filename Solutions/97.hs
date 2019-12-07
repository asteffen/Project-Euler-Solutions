-- since we are only interested in the last 10 digits of the number,
-- we can work (mod 10^10).

-- multiplies a and b (mod m).
multiplyMod m a b = a * b `mod` m

-- returns f^n(x), where f^n is f composed n times.
powerFunc f 1 x = f x
powerFunc f n x
	| even n    = powerFunc (f . f) (n `div` 2) x
	| otherwise = powerFunc f (n-1) (f x)

{-
powerFunc keeps on getting an error because of stack overflow...
so I'll just brute force it.

0.44 seconds... I have no idea how it computes it that fast.
printing (2 ^ 7830457) takes 17.16 seconds.
I guess exponentiation is fast, and printing is slow...
-}
main = print $ (28433 * 2 ^ 7830457 + 1) `mod` 10^10