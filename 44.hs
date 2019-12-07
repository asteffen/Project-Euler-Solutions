pentNums :: [Integer]
pentNums = map (\n -> n * (3*n - 1) `div` 2) [1..]

{-
I put (isPentagonal) in point-free form to increase speed.
A more understandable version is:

isPentagonal :: Integer -> Bool
isPentagonal x = fracPart == 0
	where
		n = (sqrt (24 * fromIntegral x + 1) + 1) / 6
		(_, fracPart) = properFraction n
-}
isPentagonal :: Integer -> Bool
isPentagonal = (==0) . snd . properFraction . (/6) . (+1) . sqrt . (+1) . (*24) . fromIntegral

sumDiffPent :: [(Integer, Integer)]
sumDiffPent = [(a, b) | a <- pentNumsFinite, b <- takeWhile (<a) pentNums,
	isPentagonal (a - b), isPentagonal (a + b)]
	where pentNumsFinite = take 2200 pentNums

{-
The first element in sumDiffPent is (7042750, 1560090).

pentNums !! 2166 == 7042750
pentNums !! 1019 == 1560090
-}

main :: IO ()
main = print $ uncurry (-) $ head sumDiffPent