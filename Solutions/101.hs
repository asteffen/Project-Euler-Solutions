import Matrix
import Data.List (genericLength)
import Data.Ratio ((%))

-- a polynomial expressed as a list of coefficients in increasing order.
-- e.g. a*x^2 + b^x + c is expressed as [c, b, a].
type Polynomial = [Rational]

-- constructs a coefficient matrix:
-- [1, 1, 1, 1, ...]
-- [1, 2, 4, 8, ... 2^(n-1)]
-- [1, 3, 9, 27, ..., 3^(n-1)]
-- ...
-- [1, n, n^2, n^3, ... n^(n-1)]
coefM :: Integer -> Matrix Integer
coefM n = Matrix [map (base^) [0 .. n - 1] | base <- [1..n]]

-- constructs a column matrix out of the list.
colM :: [a] -> Matrix a
colM = Matrix . map (\x -> [x])

-- evaluates a polynomial at a given rational number.
evalPoly :: Polynomial -> Rational -> Rational
evalPoly poly x = sum $ zipWith (*) poly $ map (x^) [0..]

-- the OP function described in the problem, slightly modified.
-- it takes two arguments: the polynomial and k, the degree of the result.
-- solves a system of equations using matrices:
-- A * X = B -> X = A^-1 * b, where A = coef matrix, X = variables, B = constants.
optimumPoly :: Polynomial -> Integer -> Polynomial
optimumPoly poly k = map head $ fromMatrix matX
	where
		matA = fmap (% 1) $ coefM k
		matB = colM $ map (evalPoly poly . (% 1)) [1..k]
		matX = inverse matA `mulM` matB

-- the sum of all FITs of a polynomial.
sumFits :: Polynomial -> Rational
sumFits poly = sum $ map mapF [1 .. genericLength poly - 1]
	where mapF k = evalPoly (optimumPoly poly k) (k % 1 + 1)

main :: IO ()
main = print $ sumFits $ [1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1]