import Data.List (maximumBy)
import Data.Function (on)

-- converts (contents) to [(line#, base, exponent)]
toList :: String -> [(Int, Double, Double)]
toList = insertInts . map (\s -> read $ "(" ++ s ++ ")") . lines
	where insertInts = zipWith (\i (b, e) -> (i, b, e)) [1..]

-- returns (e*log b), for comparing against other exponents.
reduce :: (Int, Double, Double) -> (Int, Double)
reduce (i, b, e) = (i, e * log b)

main :: IO ()
main = do
	contents <- readFile "99.txt"
	print $ fst $ maximumBy (compare `on` snd) $ map reduce $ toList contents