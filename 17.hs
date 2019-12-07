import Data.Char

decimalToEnglish :: Int -> String
decimalToEnglish n
	| 1  <= n && n <= 9                   = ones !! n
	| 11 <= n && n <= 19                  = teens !! (n - 10)
	| 10 <= n && n <= 99 && mod10 == 0    = tens !! div10
	| 10 <= n && n <= 99                  =
		decimalToEnglish (10 * div10) ++ "-" ++ decimalToEnglish mod10
	| 100 <= n && n <= 999 && mod100 == 0 = decimalToEnglish div100 ++ " hundred"
	| 100 <= n && n <= 999                =
		decimalToEnglish (100 * div100) ++ " and " ++ decimalToEnglish mod100
	| n == 1000                           = "one thousand"
	| otherwise                           = "error: number must be between 1 and 1000, inclusive."
	where
		div10, mod10, div100, mod100 :: Int
		(div10,  mod10)  = n `divMod` 10
		(div100, mod100) = n `divMod` 100
		
		ones, teens, tens :: [String]
		ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
		teens = ["", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen",
			"eighteen", "nineteen"]
		tens = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
			"ninety"]

main :: IO ()
main = putStrLn $ show $ sum $ map (length . filter isAlpha . decimalToEnglish) [1..1000]