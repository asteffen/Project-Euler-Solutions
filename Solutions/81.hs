-- dimensions of the matrix (constant)
dim :: Int
dim = 80

-- converts (contents) into a 2-dimensional list of Integers.
toMatrix :: String -> [[Integer]]
toMatrix = map toList . lines
	where toList str = read $ "[" ++ str ++ "]"

-- "rotates" the matrix clockwise, so that:
-- 1) the top left is now the top (start)
-- 2) the bottom left is now the bottom (end)
-- 3) the possible next step is always in the next sub-list (1 step = right or down)
rotate :: [[Integer]] -> [[Integer]]
rotate matrix = map topDiag [0..79] ++ map bottomDiag [1..79]
	where
		topDiag, bottomDiag :: Int -> [Integer]

		-- top diagonals in matrix, including middle diagonal. for row in [0..79]
		topDiag row = [(matrix !! n) !! (row - n) | n <- [0..row]]

		-- bottom diagonals in matrix. for row in [1..79]
		bottomDiag row = [(matrix !! n) !! (coordTotal - n) | n <- [row .. dim - 1]]
			where coordTotal = dim - 1 + row

-- recursively finds the minimal path starting at a certain level in the rotated matrix.
-- rotatedMatrix !! 158 is the last element.
-- rotatedMatrix !! 79 is the middle (main diagonal).
minimalPath :: [[Integer]] -> Int -> [Integer]
minimalPath rotatedMatrix 158 = rotatedMatrix !! 158 -- base case: bottom level.
minimalPath rotatedMatrix level = map currentMin [0..maxIndex]
	where
		-- (True) if the level is on or below the middle
		inBottomHalf :: Bool
		inBottomHalf = level >= 79
		
		-- minimal paths for each index in the next level.
		next :: [Integer]
		next = minimalPath rotatedMatrix (level + 1)
		
		-- the current elements in the rotated matrix.
		thisLevel :: [Integer]
		thisLevel = rotatedMatrix !! level
		
		-- highest index of the current level.
		maxIndex :: Int
		maxIndex = length thisLevel - 1
		
		-- minimum path from the current index and level.
		currentMin :: Int -> Integer
		currentMin index
			| not inBottomHalf  = thisLevel !! index + min (next !! index) (next !! (index + 1))
			-- there is only one choice at either end in the bottom half.
			| index == 0        = thisLevel !! 0 + next !! 0
			| index == maxIndex = thisLevel !! maxIndex + next !! (maxIndex - 1)
			| otherwise         = thisLevel !! index + min (next !! (index - 1)) (next !! index)

main :: IO ()
main = do
	contents <- readFile "81.txt"
	let
		matrix = toMatrix contents
		rotated = rotate matrix
	print $ head $ minimalPath rotated 0