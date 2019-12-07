import Data.Char (digitToInt)
import Data.List (delete)

{-
-- I didn't end up using these after all, but I'll save them here anyway.

isInt :: Double -> Bool
isInt = (== 0) . snd . properFraction

-- the tests for 3- to 6-gonal numbers come from wikipedia.
-- I derived the tests for 7- and 8- gonal numbers.
is3gon, is4gon, is5gon, is6gon, is7gon, is8gon :: Integer -> Bool
is3gon x = isInt $ sqrt $ 8 * fromIntegral x + 1
is4gon x = isInt $ sqrt $ fromIntegral x
is5gon x = isInt $ (/6) $ sqrt (24 * fromIntegral x + 1) + 1
is6gon x = isInt $ (/4) $ sqrt (8 * fromIntegral x + 1) + 1
is7gon x = isInt $ (/10) $ sqrt (40 * fromIntegral x + 9) + 3
is8gon x = isInt $ (/6) $ sqrt (12 * fromIntegral x + 4) + 2
-}

-- converts an integer into a list of digits, e.g.
-- 4293 -> [4, 2, 9, 3]
intToDigits :: Integer -> [Int]
intToDigits = map digitToInt . show

-- map (f) over the natural numbers, then take only the 4-digit numbers.
overN_4digits :: (Integer -> Integer) -> [Integer]
overN_4digits f = takeWhile (< 10000) $ dropWhile (< 1000) $ map f [1..]

list3gons, list4gons, list5gons, list6gons, list7gons, list8gons :: [Integer]
list3gons = overN_4digits (\n -> n * (n + 1) `div` 2)
list4gons = overN_4digits (\n -> n * n)
list5gons = overN_4digits (\n -> n * (3*n - 1) `div` 2)
list6gons = overN_4digits (\n -> n * (2*n - 1))
list7gons = overN_4digits (\n -> n * (5*n - 3) `div` 2)
list8gons = overN_4digits (\n -> n * (3*n - 2))

-- list of (numSides, num) for each polygonal number above.
listNgons :: [(Integer, Integer)]
listNgons = concat $ zipWith (\n xs -> zip (repeat n) xs) [3..8]
	[list3gons, list4gons, list5gons, list6gons, list7gons, list8gons]

-- returns (True) if the last 2 digits of (a) are the first 2 digits of (b),
-- assuming (a) and (b) are both 4-digit numbers.
isCyclicTo :: Integer -> Integer -> Bool
isCyclicTo a b = drop 2 (intToDigits a) == take 2 (intToDigits b)

-- splits (xs) into sub-lists of length (n).
splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs = current : splitInto n next
	where (current, next) = splitAt n xs

-- list of all lists of (n) 4-digit numbers with the 3 properties described in the problem.
solve :: Integer -> [[Integer]]
solve n = filter (\xs -> isCyclicTo (last xs) (head xs)) $ splitInto
	(fromIntegral n) $ concatMap (solve' [] [3 .. 2 + n]) [1010..1099]
	where
		solve' :: [Integer] -> [Integer] -> Integer -> [Integer]
		solve' acc [] _ = acc
		solve' acc nsLeft current = concat $ map next $ filter isEligible listNgons
			where
				isEligible (sides, num) = sides `elem` nsLeft && isCyclicTo current num
				next (sides, num) = solve' (acc ++ [num]) (delete sides nsLeft) num
		
main :: IO ()
main = print $ sum $ head $ solve 6