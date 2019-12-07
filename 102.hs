import Text.ParserCombinators.Parsec

data Vector = Vector Int Int
	deriving (Show)

data Triangle = Triangle Vector Vector Vector
	deriving (Show)

parseTriangle :: Parser Triangle
parseTriangle = do
	x1 <- noCommas
	comma
	y1 <- noCommas
	comma
	x2 <- noCommas
	comma
	y2 <- noCommas
	comma
	x3 <- noCommas
	comma
	y3 <- noCommas
	let [x1', y1', x2', y2', x3', y3'] = map read [x1, y1, x2, y2, x3, y3]
	return $ Triangle (Vector x1' y1') (Vector x2' y2') (Vector x3' y3')
	where
		noCommas = many $ noneOf ","
		comma = char ','

lineToTriangle :: String -> Triangle
lineToTriangle str = case parse parseTriangle "triangle" str of
	Left err  -> error $ show err
	Right val -> val

-- dot product
dot :: Vector -> Vector -> Int
(Vector x1 y1) `dot` (Vector x2 y2) = x1 * x2 + y1 * y2

magnitude :: Vector -> Double
magnitude (Vector x y) = sqrt $ fromIntegral $ x * x + y * y

-- returns the angle between two vectors, in radians.
-- the angle returned will be in [0, pi].
angle :: Vector -> Vector -> Double
angle a b = acos $ fromIntegral (a `dot` b) / (magnitude a * magnitude b)

-- uses a simple, but inefficient method.
-- a point P is in triangle ABC iff the sum of the 3 angles between
-- vectors PA, PB, and PC is equal to 2*pi.
triangleContainsOrigin :: Triangle -> Bool
triangleContainsOrigin (Triangle a b c) =
	(angle a b + angle b c + angle c a) `dblEql` (2 * pi)

-- returns (True) if (a) and (b) are within (epsilon) of each other.
dblEql :: Double -> Double -> Bool
a `dblEql` b = abs (a - b) <= epsilon
	where epsilon = 1e-8

main :: IO ()
main = do
	contents <- readFile "102.txt"
	let triangles = map lineToTriangle $ lines contents
	
	print $ length $ filter triangleContainsOrigin triangles