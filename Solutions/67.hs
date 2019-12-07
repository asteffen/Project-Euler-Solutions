-- note: very similar to problem 18.

import System.IO

toIntegerList :: String -> [[Integer]]
toIntegerList contents = map (map stringToInt) stringLists
	where
		stringLists :: [[String]]
		stringLists = map words $ lines contents
		
		stringToInt :: String -> Integer
		stringToInt s = read s :: Integer

-- returns potential maximum paths starting from a certain level in the triangle.
maxPaths :: [[Integer]] -> Int -> [Integer]
maxPaths grid level
	| level == length_1 = grid !! length_1
	| otherwise         = [ this_level !! n + max (next_level !! n) (next_level !! (n + 1))
		| n <- [0 .. level] ]
	where
		length_1   = length grid - 1
		this_level = grid !! level
		next_level = maxPaths grid (level + 1)

main :: IO ()
main = do
	handle <- openFile "67.txt" ReadMode
	contents <- hGetContents handle
	let grid = toIntegerList contents
	
	putStrLn $ show $ head $ maxPaths grid 0
		
	hClose handle