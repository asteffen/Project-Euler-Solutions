{- Note:
The (solve) function runs very slowly.

It took about 1 hour (3615.19 seconds) to calculate (solutions).
-}

import Data.Char (intToDigit)
import Data.List (transpose, nub, intersperse)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (mplus)

--------------------------------------------------------------------------------
-- Solving
--------------------------------------------------------------------------------

-- Sudoku type: a list of 81 Ints.
-- Order is left-to-right, top-to-bottom.
-- Blanks are represented by 0.
type Sudoku = [Int]

-- Indices which must contain distinct elements.
groups, rows, cols, squares :: [[Int]]
groups = rows ++ cols ++ squares
rows = [map (\b -> 9*a + b) [0..8] | a <- [0..8]]
cols = transpose rows
squares = [map (+n) [0, 1, 2, 9, 10, 11, 18, 19, 20] |
	n <- [0, 3, 6, 27, 30, 33, 54, 57, 60]]

isValid :: Sudoku -> Bool
isValid sudoku = all rowIsValid [map (sudoku !!) group | group <- groups]
	where
		rowIsValid = distinctElems . filter (/= 0)
		distinctElems xs = xs == nub xs

replaceFirst :: (Eq a) => [a] -> a -> a -> [a]
replaceFirst [] _ _ = []
replaceFirst (x:xs) old new
	| x == old  = new : xs
	| otherwise = x : replaceFirst xs old new

solve :: Sudoku -> Sudoku
solve sudoku
	| 0 `notElem` sudoku = sudoku
	| otherwise = concatMap solve possibles
	where
		possibles = filter isValid $ map (replaceFirst sudoku 0) [1..9]

--------------------------------------------------------------------------------
-- Experimental
--------------------------------------------------------------------------------

-- a more efficient solve function, using the (mplus) function.
solve2 :: Sudoku -> Maybe Sudoku
solve2 sudoku
	| 0 `notElem` sudoku = Just sudoku
	| otherwise          = foldl mplus Nothing $ map solve2 possibles
	where
		possibles = filter isValid $ map (replaceFirst sudoku 0) [1..9]

data IntTree = Node Int [IntTree]
	deriving (Show)

possibleTree :: Sudoku -> IntTree
possibleTree sudoku
	| 0 `notElem` sudoku = Node (-1) []
	| otherwise = Node (-1) $ map subTree possibles
		where
			possibles = filter (isValid . snd) $ map (\n -> (n, replaceFirst sudoku 0 n)) [1..9]
			subTree (n, poss) = Node n $ solve poss

--------------------------------------------------------------------------------
-- Parsing / main
--------------------------------------------------------------------------------

contents :: String
contents = unsafePerformIO $ readFile "96.txt"

parseContents :: String -> [Sudoku]
parseContents "" = []
parseContents contents = sudoku : parseContents (unlines rest)
	where
		(first10lines, rest) = splitAt 10 $ lines contents
		toList str = read $ "[" ++ intersperse ',' str ++ "]"
		sudoku = toList $ concat $ tail first10lines

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

-- returns the top left 3 digits of a sudoku puzzle.
topLeftNum :: Sudoku -> Integer
topLeftNum = digitsToInt . take 3

solutions :: [Sudoku]
solutions = map solve $ parseContents contents

main :: IO ()
main = print $ sum $ map topLeftNum solutions

--------------------------------------------------------------------------------
-- Samples for testing
--------------------------------------------------------------------------------

sample :: Sudoku
sample =
	[4,8,3,9,2,1,6,5,7
	,9,6,7,3,4,5,8,2,1
	,2,5,1,8,7,6,4,9,3
	,5,4,8,1,3,2,9,7,6
	,7,2,9,5,6,4,1,3,8
	,1,3,6,7,9,8,2,4,5
	,3,7,2,6,8,9,5,1,4
	,8,1,4,2,5,3,7,6,9
	,6,9,5,4,1,7,3,8,2
	]

-- from the problem
unsolved :: Sudoku
unsolved =
	[0,0,3,0,2,0,6,0,0
	,9,0,0,3,0,5,0,0,1
	,0,0,1,8,0,6,4,0,0
	,0,0,8,1,0,2,9,0,0
	,7,0,0,0,0,0,0,0,8
	,0,0,6,7,0,8,2,0,0
	,0,0,2,6,0,9,5,0,0
	,8,0,0,2,0,3,0,0,9
	,0,0,5,0,1,0,3,0,0
	]

-- from Wikipedia
unsolved2 :: Sudoku
unsolved2 =
	[5,3,0,0,7,0,0,0,0
	,6,0,0,1,9,5,0,0,0
	,0,9,8,0,0,0,0,6,0
	,8,0,0,0,6,0,0,0,3
	,4,0,0,8,0,3,0,0,1
	,7,0,0,0,2,0,0,0,6
	,0,6,0,0,0,0,2,8,0
	,0,0,0,4,1,9,0,0,5
	,0,0,0,0,8,0,0,7,9
	]