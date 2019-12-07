{-
The number of rectangles contained in an x-by-y rectangle is

	(binom (x+1) 2) * (binom (y+1) 2)

This is because there are (binom (x+1) 2) ways to pick the 2 lines that form the
left and right sides of the rectangle, and (binom (y+1) 2) ways to pick the 2
lines that form the top and bottom sides of the rectangle.

So the answer is (a * b), where (binom a 2) * (binom b 2) is closest to 2 million,
and a and b are integers greater than or equal to 2.
-}

import Data.List (minimumBy)
import Data.Function (on)

-- n choose k.
-- After I found the solution, I realized I didn't really need to define this.
-- Because k is always 2 in this problem, I could have just used n*(n+1)/2.
binom :: Integer -> Integer -> Integer
binom n k = product [n-k+1 .. n] `div` product [1..k]

-- positive difference between n and 2 million.
diff2million :: Integer -> Integer
diff2million = abs . subtract 2000000

-- number of rectangles contained in an x-by-y rectangle
countRect :: Integer -> Integer -> Integer
countRect x y = binom (x+1) 2 * binom (y+1) 2

-- all possible numbers of rectangles contained in an x-by-y rectangle,
-- where x and y are both between 1 and 100.
-- (I guessed the dimensions of the answer would be less than 100)
-- Only calculated for x <= y, to avoid duplicates.
possibleCounts :: [(Integer, Integer, Integer)]
possibleCounts = [(x, y, countRect x y) | x <- [1..100], y <- [x..100]]

main :: IO ()
main = print $ fst $ minimumBy (compare `on` snd) possibleDiffs
	where possibleDiffs = map (\(x, y, count) -> (x * y, diff2million count)) possibleCounts