-- Interpreted: 25 sec

-- Quadratic irrational of the form ((a + sqrt b) / c).
data Quad = Quad Integer Integer Integer
	deriving (Eq)

instance Show Quad where
	show (Quad a b c) = "(" ++ a' ++ " " ++ sign ++ " sqrt " ++ b' ++ ") / " ++ c'
		where
			a' = show a
			b' = show $ abs b
			c' = show c
			sign = if b > 0 then "+" else "-"

simplify :: Quad -> Quad
simplify q@(Quad a b c)
	| c < 0 = Quad (-a) (-b) (-c)
	| otherwise = q

-- It seems that (a * a - b) is always divisible by (c), so a simple division
-- is done instead of computing GCD.
recip' :: Quad -> Quad
recip' (Quad a b c) = Quad a (-b) ((a * a - b) `div` c)

-- Returns the floor of a (Quad).
floor' :: Quad -> Integer
floor' (Quad a b c) = if b > 0
	then floor $ (a' + sqrt b') / c'
	else floor $ (a' - sqrt (-b')) / c'
		where [a', b', c'] = map fromIntegral [a, b, c]

-- Subtracts an (Integer) from a (Quad).
quadMinusInt :: Quad -> Integer -> Quad
quadMinusInt (Quad a b c) d = Quad (a - d * c) b c

-- Converts a (Quad) into a continued fraction, represented as a list of
-- Integers. The list goes to the end of the first repetition. The repeating
-- portion is all but the first value in the list.
toContFrac :: Quad -> [Integer]
toContFrac = toContFrac' []
	where
		-- Checks if (q) is in the previously computed values (qList). If it is, then
		-- it stops, because the end of the first repetition has been reached.
		toContFrac' :: [Quad] -> Quad -> [Integer]
		toContFrac' qList q = if q `elem` qList
			then []
			else f : toContFrac' (q : qList) (simplify $ recip' $ quadMinusInt q f)
			where f = floor' q

-- Returns the continued fraction representation of (sqrt n).
sqrtToContFrac :: Integer -> [Integer]
sqrtToContFrac n = case intSqrt n of
	Just sqrtN -> [sqrtN]
	Nothing -> toContFrac $ Quad 0 n 1

intSqrt :: Integer -> Maybe Integer
intSqrt n = if fPart == 0 then Just iPart else Nothing
	where (iPart, fPart) = properFraction $ sqrt $ fromIntegral n

period :: Integer -> Int
period = length . tail . sqrtToContFrac

main :: IO ()
main = print $ length $ filter odd $ map period [1..10000]