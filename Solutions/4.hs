-- i forgot i could have simply done:
-- isPalindrome str = str == reverse str
isPalindrome :: String -> Bool
isPalindrome str
	| len == 1             = True
	| head str == last str = len == 2 || isPalindrome (middle str)
	| otherwise            = False
	where
		middle = init . tail
		len = length str

numIsPalindrome :: Integer -> Bool
numIsPalindrome = isPalindrome . show

main :: IO ()
main = putStrLn $ show $ maximum listPalindromes
	where listPalindromes =
		[ c | a <- [100..999], b <- [a..999], let c = a * b, numIsPalindrome c]