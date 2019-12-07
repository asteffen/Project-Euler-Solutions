import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Data.List (findIndex, findIndices)
import Data.Char (isDigit)

wordList :: [String]
wordList = read $ "[" ++ unsafePerformIO (readFile "98.txt") ++ "]"

squares :: [String]
squares = map (show . (^2)) [1..]

nDigitSquares :: Int -> [String]
nDigitSquares n = filter ((== n) . length) $ takeWhile ((<= n) . length) squares

-- given: 2 words, w1 and w2
--        num, the string representation of a square number.
--             the digits in (num) correspond to the letters in (w1).
-- returns: w2, partially substituted with numbers.
substitute :: String -> String -> String -> String
substitute w1 w2 num = replace w2
	where
		replace :: String -> String
		replace [] = []
		replace (c:cs) = case findIndex (== c) w1 of
			Nothing -> c : replace cs
			Just i  -> num !! i : replace cs

-- returns (True) if each character in one string corresponds to on in the
-- other string.
strEquiv :: String -> String -> Bool
strEquiv = (==) `on` indices
	where
		indices :: String -> [[Int]]
		indices str = map (\char -> findIndices (== char) str) str

-- returns (True) if each digit in the first string is identical to the digit
-- at the same index in the seconds string.
digitsSame :: String -> String -> Bool
digitsSame word num = and $ zipWith zipFunc word num
	where zipFunc c1 c2 = not (isDigit c1) || c1 == c2

-- returns a list of all squares that a string could represent
possibleSq :: String -> [String]
possibleSq str = filter filterFunc $ nDigitSquares $ length str
	where filterFunc num = strEquiv str num && digitsSame str num

-- given two words, computes their "square anagram word pairs"
-- the order of the arguments matters, but it shouldn't.
anagrams word1 word2 = validMatches
	where
		poss = possibleSq word1
		subs = map (\num -> (num, substitute word1 word2 num)) poss
		matches = map (\(num, word) -> (num, possibleSq word)) subs
		validMatches :: [(Integer, Integer)]
		validMatches = map (\(str, xs) -> (read str, read $ last xs)) $
			filter (not . null . snd) matches