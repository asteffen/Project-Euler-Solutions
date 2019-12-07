type Triplet = (Double, Double, Double)

-- returns (True) if the argument is a Cardano triplet.
isCardano :: Triplet -> Bool
isCardano (a, b, c) = a*a - b*b*c == ((1 - 2*a)/3)**3

-- returns all (a, b, c) such that a, b, c are in [1..n]
tripletsUpTo :: Double -> [Triplet]
tripletsUpTo n = [(a, b, c) | a <- range, b <- range, c <- range]
	where range = [1..n]