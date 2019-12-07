
import Data.List (transpose)
import Data.Maybe (isJust, fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad

type Matrix = [[Integer]]
type Index = (Int, Int)
--{-
-- matrix indexing so that (1, 1) is the top left element
indexMatrix :: Matrix -> Index -> Integer
indexMatrix matrix (y, x) = matrix !! (y - 1) !! (x - 1)

-- given a matrix and an index, returns the smallest sum possible from that
-- index to the next row.
nextVal :: Matrix -> Index -> Integer
nextVal matrix index@(y, x) = (+ current) $ minimum $ map fromJust $
	filter isJust [rightTotal, upTotal, downTotal]
	where
		dim = length matrix
		current = indexMatrix matrix index
		
		right = (y, x+1)
		up = (y-1, x)
		upRight = (y-1, x+1)
		down = (y+1, x)
		downRight = (y+1, x+1)
		
		[rightVal, upVal, upRightVal, downVal, downRightVal] = map
			(indexMatrix matrix) [right, up, upRight, down, downRight]
		
		rightTotal = Just rightVal
		upTotal = if y <= 1
			then Nothing
			else Just $ upVal + upRightVal
		downTotal = if y >= dim
			then Nothing
			else Just $ downVal + downRightVal

-- replaces an element at the specified index
-- assumes list indices start at 1.
replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 1 rep (_:xs) = rep : xs
replaceAt index rep (x:xs) = x : replaceAt (index - 1) rep xs

-- given a matrix and a column, maps (nextVal) on each element in the column
-- simultaneously so that each element is as if it were the first one mapped.
reduceCol :: Matrix -> Int -> Matrix
reduceCol matrix x = map init $ transpose $ replaceAt x newCol $ transpose matrix
	where
		dim = length matrix
		newCol = map (\y -> nextVal matrix (y, x)) [1..dim]

-- returns the minimal sum of a matrix
minimalSum :: Matrix -> Integer
minimalSum matrix = minimum $ map head reducedMatrix
	where
		dim = length matrix
		reducedMatrix = foldl reduceCol matrix [dim-1, dim-2 .. 1]

ans1 = minimalSum mat
--}

------ new?

-- converts (contents) into a 2-dimensional list of Integers.
toMatrix :: String -> Matrix
toMatrix = map toList . lines
	where toList str = read $ "[" ++ str ++ "]"

mat :: Matrix
mat = toMatrix $ unsafePerformIO $ readFile "81.txt"

smat :: Matrix
smat =
	[[131,673,234,103, 18]
	,[201, 96,342,965,150]
	,[630,803,746,422,111]
	,[537,699,497,121,956]
	,[805,732,524, 37,331]
	]

dropAllBut2 xs = drop (length xs - 2) xs
takeAllBut2 xs = take (length xs - 2) xs

f matrix row = takeAllBut2 curRow ++ [minSum]
	where
		curRow = matrix !! row
		aboveRow = if row == 0 then Nothing else Just $ matrix !! (row - 1)
		belowRow = if row == length matrix - 1 then Nothing else Just $ matrix !! (row + 1)
		
		cur2@(curElem:_) = dropAllBut2 curRow
		curSum = Just $ sum cur2
		aboveSum = liftM (sum . (curElem :) . dropAllBut2) aboveRow
		belowSum = liftM (sum . (curElem :) . dropAllBut2) belowRow
		minSum = minimum [s | Just s <- [aboveSum, curSum, belowSum]]

reduce matrix = map (f matrix) [0 .. length matrix - 1]

ans2 = minimal mat

minimal = minimum . concat . until ((== 1) . length . head) reduce