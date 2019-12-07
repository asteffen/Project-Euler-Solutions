import Data.List (maximumBy)
import Data.Function (on)

--------------------------------------------------------------------------------
-- Simple functions to test optimized functions
--------------------------------------------------------------------------------

-- product of (n) split into (k) equal parts.
p :: Integer -> Integer -> Double
p n k = (fromIntegral n / fromIntegral k) ^ k

-- the number k such that (p n k) is maximized.
m :: Integer -> Integer
m n = fst $ maximumBy (compare `on` snd) $ map (\k -> (k, p n k)) [1..n]

--------------------------------------------------------------------------------
-- Optimized functions to determine answer
--------------------------------------------------------------------------------

{-
a much faster version of the (m) function.

derivation, using calculus:

given N, we wish to maximize the function f(x) = (N/x)^x.
the derivitive of f(x) is f'(x) = (N/x)^x * (ln(N/x) - 1)
set f'(x) = 0 to find the global maximum -> x = N/e.

however, we only want integer x. because the graph of f(x)
is nearly symmetric around its maximum, we can simply round x = N/e.
-}
m2 :: Integer -> Integer
m2 n = round $ fromIntegral n / e
	where e = exp 1

-- given the denominator of a fraction (in simplest form),
-- returns (True) if the decimal representation of the fraction terminates.
terminates 1 = True
terminates n
	| mod2 == 0 = terminates div2
	| mod5 == 0 = terminates div5
	| otherwise = False
	where
		(div2, mod2) = n `divMod` 2
		(div5, mod5) = n `divMod` 5

-- D(N) = -N, if M(N) is a terminating decimal
--         N, otherwise
d :: Integer -> Integer
d n = if terminates $ mn `div` gcd n mn
	then negate n
	else n
	where mn = m2 n

main :: IO ()
main = print $ sum $ map d [5..10000]