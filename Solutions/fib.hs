import Data.Ratio
import Matrix
import Data.List
import Data.Bits
 
fib :: Int -> Integer
fib n = snd . foldl' fib' (1, 0) $ dropWhile not $
            [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib' (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g

-- infinite list of fibonacci numbers.
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{-------------------------------------------------------------------------------
Summary of new functions

nthFib2 is faster than nthFib.
using the (fibs) list is faster when calculating many values.

-------------------------------------------------------------------------------}

-- a data type that stores the first 9 digits and last 9 digits of a number.
--type FLDigits

-- numbers of the form a + (sqrt 5)*b
data Root5 = Root5 Rational Rational
	deriving (Show, Eq)

instance Num Root5 where
	(Root5 a b) + (Root5 c d) = Root5 (a + c) (b + d)
	(Root5 a b) * (Root5 c d) = Root5 (a*c + 5*b*d) (a*d + b*c)
	negate (Root5 a b) = Root5 (-a) (-b)
	fromInteger a = Root5 (a % 1) 0
	
	-- placeholders
	abs = id
	signum = id

instance Fractional Root5 where
	(Root5 a b) / (Root5 c d) = Root5 ((a*c - 5*b*d) / denom) ((b*c - a*d) / denom)
		where denom = c*c - 5*d*d
	
	-- placeholder
	fromRational r = Root5 0 0

-- the golden ratio
phi :: Root5
phi = Root5 (1 % 2) (1 % 2)

-- returns F_n, the nth fibonacci number.
-- uses the exact formula, and manipulates irrational numbers.
nthFib n = numerator a
	where -- a will always be an integer
		(Root5 a _) = (phi ^ n - (1 - phi) ^ n) / (Root5 0 1)

-----

matF :: Matrix Integer
matF = Matrix [[1, 1], [1, 0]]

-- returns F_n, the nth fibonacci number.
-- uses matrix exponentiation
nthFib2 0 = 0
nthFib2 n = head $ head $ fromMatrix $ matF ^ (n - 1)