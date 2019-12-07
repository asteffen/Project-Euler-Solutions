-- infinite list of hexagonal numbers
hexagons :: [Integer]
hexagons = map (\n -> n * (2*n - 1)) [1..]

-- returns (True) if a Double is an integer, e.g. 4.0
isInteger :: Double -> Bool
isInteger = (==0) . snd . properFraction

-- returns (True) if a number is pentagonal
isPentagonal :: Integer -> Bool
isPentagonal x = isInteger n
	where n = (sqrt (24 * fromIntegral x + 1) + 1) / 6

-- all hexagonal numbers are also triangular, so we don't need to test if
-- the number is triangular.
main :: IO ()
main = print $ head $ filter isPentagonal $ dropWhile (<= 40755) hexagons