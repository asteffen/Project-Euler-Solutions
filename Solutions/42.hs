toList :: String -> [String]
toList contents = read $ "[" ++ contents ++ "]"

-- 64 == fromEnum 'A' - 1
charToInt :: Char -> Int
charToInt = subtract 64 . fromEnum

triangularNums :: [Integer]
triangularNums = [n * (n+1) `div` 2 | n <- [1..]]

wordScore :: String -> Integer
wordScore = fromIntegral . sum . map charToInt

isTriangleWord :: String -> Bool
isTriangleWord word = (score `elem`) $ takeWhile (<= score) triangularNums
	where score = wordScore word

main :: IO ()
main = do
	contents <- readFile "42.txt"
	print $ length $ filter isTriangleWord $ toList contents