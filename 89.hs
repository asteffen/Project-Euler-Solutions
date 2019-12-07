type RomanNum = String

-- the values of all roman numeral "digits", including subtractive pairs.
-- used for intToRoman.
values :: [(RomanNum, Int)]
values =
	[("M",  1000)
	,("CM", 900)
	,("D",  500)
	,("CD", 400)
	,("C",  100)
	,("XC", 90)
	,("L",  50)
	,("XL", 40)
	,("X",  10)
	,("IX", 9)
	,("V",  5)
	,("IV", 4)
	,("I",  1)
	]

-- returns the first element in the table above, such that
-- the number in that element is less than or equal to n.
firstElem :: Int -> (RomanNum, Int)
firstElem n = values !! firstIndex
	where firstIndex = until ((<= n) . snd . (values !!)) succ 0

-- converts an Int into a minimal roman numeral.
intToRoman :: Int -> RomanNum
intToRoman 0 = ""
intToRoman n = symbol ++ intToRoman (n - val)
	where (symbol, val) = firstElem n

-- returns the value of a roman numeral "digit".
getValue :: RomanNum -> Int
getValue digit = val
	where Just val = lookup digit values

-- converts a valid roman numeral to an Int.
-- the roman numeral doesn't need to be minimal.
romanToInt :: RomanNum -> Int
romanToInt (a:b:rest)
	| valA < valB = valB - valA + romanToInt rest
	| otherwise   = valA + romanToInt (b:rest)
	where
		valA = getValue [a]
		valB = getValue [b]
romanToInt [a] = getValue [a]
romanToInt "" = 0

-- returns the number of characters saved by writing the roman numeral in
-- minimal form. (returns 0 for roman numerals already in minimal form.)
charsSaved :: RomanNum -> Int
charsSaved rn = length rn - (length . intToRoman . romanToInt) rn

main :: IO ()
main = do
	contents <- readFile "89.txt"
	print $ sum $ map charsSaved $ lines contents