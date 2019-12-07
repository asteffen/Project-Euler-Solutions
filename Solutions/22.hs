import Data.List (sort)

toList :: String -> [String]
toList contents = read $ "[" ++ contents ++ "]"

-- 64 == fromEnum 'A' - 1
charToInt :: Char -> Int
charToInt = subtract 64 . fromEnum

sumOfScores :: [String] -> Integer
sumOfScores sortedList = sum $ map score zippedList
	where
		zippedList :: [(Integer, String)]
		zippedList = zip [1..] sortedList
		
		score :: (Integer, String) -> Integer
		score (index, name) = index * (fromIntegral . sum . map charToInt) name

main :: IO ()
main = do
	contents <- readFile "22.txt"
	print $ sumOfScores $ sort $ toList $ contents