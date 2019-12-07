import System.IO

-- dimensions (both width and height) of the grid
dim :: Int
dim = 20

toIntList :: String -> [[Int]]
toIntList contents = map (map stringToInt) stringLists
	where
		stringLists :: [[String]]
		stringLists = map words $ lines contents
		
		stringToInt :: String -> Int
		stringToInt s = read s :: Int

horizontalProducts :: [[Int]] -> [Int]
horizontalProducts grid = [ product $ map (row !!) [startCol..startCol+3]
	| row <- grid, startCol <- [0..dim-4] ]

verticalProducts :: [[Int]] -> [Int]
verticalProducts grid = [ product $ map (\row -> grid !! row !! col) [startRow..startRow+3]
	| startRow <- [0..dim-4], col <- [0..dim-1] ]

diagonalProducts :: [[Int]] -> [Int]
diagonalProducts grid = [ product $ map (\n -> grid !! (startRow + n) !! (startCol + n)) [0..3]
	| startRow <- [0..dim-4], startCol <- [0..dim-4] ]

antidiagonalProducts :: [[Int]] -> [Int]
antidiagonalProducts grid = [ product $ map (\n -> grid !! (startRow - n) !! (startCol + n)) [0..3]
	| startRow <- [3..dim-1], startCol <- [0..dim-4] ]

maxProduct :: [[Int]] -> Int
maxProduct grid = maximum
	(  horizontalProducts   grid
	++ verticalProducts     grid
	++ diagonalProducts     grid
	++ antidiagonalProducts grid
	)

main :: IO ()
main = do
	handle <- openFile "11.txt" ReadMode
	contents <- hGetContents handle
	
	let grid = toIntList contents
	putStrLn $ show $ maxProduct grid
	
	hClose handle