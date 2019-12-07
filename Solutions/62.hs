import Data.List (sort, sortBy, groupBy)
import Data.Function (on)

cubes :: [Integer]
cubes = map (^3) [1..]

nDigitCubes :: Integer -> [Integer]
nDigitCubes n = takeWhile (< 10 * power10) $ dropWhile (< power10) cubes
	where power10 = 10 ^ (n - 1)

-- returns a list of all: lists of 5 cubes such that
-- all of them have (n) digits, and are permutations of each other.
permCubes :: Integer -> [[Integer]]
permCubes = filter ((== 5) . length) . map (map fst) . groupBy ((==) `on` snd) .
	sortBy (compare `on` snd) . map (\n -> (n, sort $ show n)) . nDigitCubes

main :: IO ()
main = print $ head $ concatMap permCubes [1..]