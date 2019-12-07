import System.IO

toIntegerList :: String -> [Integer]
toIntegerList contents = map read $ lines contents

first10digits :: Integer -> String
first10digits n = take 10 $ show n

main :: IO ()
main = do
	handle <- openFile "13.txt" ReadMode
	contents <- hGetContents handle
	
	let answer = first10digits $ sum $ toIntegerList contents
	putStrLn answer
	
	hClose handle