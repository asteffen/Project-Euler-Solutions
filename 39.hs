import Data.List (sort, group, maximumBy)
import Data.Function (on)

{-
since a < b < c, a + b + c < 3 * a.

if a == 400, then a + b + c < 1200, which is a good bound to find the
triplets with sum under 1000.
-}
triplets :: [[Integer]]
triplets = [[a, b, ipart] | a <- [1..400], b <- [a..400],
	let
		c = sqrt $ fromIntegral $ a*a + b*b
		(ipart, fpart) = properFraction c,
	fpart == 0]

main :: IO ()
main = print $ fst $ maximumBy (compare `on` snd) $
	map (\xs -> (head xs, length xs)) $ group $ sort $ map sum $ triplets

triplets2 :: [[Integer]]
triplets2 = [[a, b, c] | n <- [1..], m <- [n+1..],
	let
		a = 2 * m * n
		b = m * m - n * n
		c = m * m + n * n]