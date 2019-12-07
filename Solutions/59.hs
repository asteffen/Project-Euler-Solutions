{- Notes

I was initially testing using isCorrectKey, but got no correct results.
So I made the numWords function, and sorted. Much slower, but I got an answer.

42.txt contains a list of common English words.

Out of curiosity, "god" is the password.

Executes in ~46 sec.
-}

import Data.Char (chr, ord, toUpper, isAlpha)
import Data.Bits (xor)
import Data.List (maximumBy)
import Data.Function (on)

-- I included this so I don't have to use a type signature when using (xor).
xorInts :: Int -> Int -> Int
xorInts = xor

toList :: (Read a) => String -> [a]
toList str = read $ "[" ++ str ++ "]"

-- list of all strings made of 3 lowercase characters
-- represents the keys as lists of ASCII codes, e.g. 65 instead of 'A'.
possibleKeys :: [[Int]]
possibleKeys = [[x, y, z] | x <- range, y <- range, z <- range]
	where range = [ord 'a' .. ord 'z']

isWord :: [String] -> String -> Bool
isWord wordList str = modifiedStr `elem` wordList
	where
		-- converted to all uppercase, stripped of non-letters
		modifiedStr = [toUpper c | c <- str, isAlpha c]

-- decodes (cipher) using (key)
decode :: [Int] -> [Int] -> String
decode cipher key = map chr $ zipWith xorInts cipher $ cycle key

-- returns (True) if, when (cipher) is decoded using (key),
-- every string of characters between spaces in the result
-- is a common English word.
isCorrectKey :: [String] -> [Int] -> [Int] -> Bool
isCorrectKey wordList cipher key = all (isWord wordList) $ words $ decode cipher key

-- returns number of words when decoding (cipher) using (key).
numWords :: [String] -> [Int] -> [Int] -> Int
numWords wordList cipher key = length $ filter (isWord wordList) $ words $ decode cipher key

main :: IO ()
main = do
	-- initially was named "words", took me forever to find out it was name clashing
	-- with the stardard function with the same name.
	engWords <- readFile "42.txt"
	cipher <- readFile "59.txt"
	
	let
		wordList :: [String]
		wordList = toList engWords
		
		cipherList :: [Int]
		cipherList = toList cipher
		
		key :: [Int]
		key = fst $ maximumBy (compare `on` snd) $
			map (\k -> (k, numWords wordList cipherList k)) possibleKeys
		
		originalText :: String
		originalText = decode cipherList key
	
	print $ sum $ map ord originalText