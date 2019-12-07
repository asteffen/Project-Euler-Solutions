-- Elliptic Curve Factorization

module ECF where

-- input: 2 Integers (a) and (b)
-- output: the solution (x, y) to the equation (ax + by = gcd a b)
extendedGCD :: Integer -> Integer -> (Integer, Integer)
extendedGCD a b
	| r == 0    = (0, 1)
	| otherwise = (y, x - y * q)
	where
		(x, y) = extendedGCD b r
		(q, r) = a `quotRem` b

-- input: 2 Integers (a) and (m). they must be coprime for the output to be correct.
-- output: (a^-1) = the modular multiplicative inverse of (a) mod (m).
modInv :: Integer -> Integer -> Integer
modInv a m = fst $ extendedGCD a m

data Point = Point Integer Integer | Inf
	deriving (Eq, Show)

-- (a, b, p)
-- y^2 = x^3 + ax + b
type EllipticCurve = (Integer, Integer, Integer)

groupOp (a, b, p) p1 p2
	| p1 == Inf = p2
	| p2 == Inf = p1
	| (x1, y1) == (x2, -y2) = Inf
	| otherwise = Point x3 y3
	where
		Point x1 y1 = p1
		Point x2 y2 = p2
		
		lambda = if x1 == x2
			then modInv (2 * y1) p * (3 * x1 * x1 + a) `mod` p
			else modInv (x1 - x2) p * (y1 - y2) `mod` p
		
		x3 = (lambda * lambda - x1 - x2) `mod` p
		y3 = (lambda * (x1 - x3) - y1) `mod` p