{-
There is a minimum of 8 digits needed: 01236789
-}

import Data.List (permutations)
import Data.Char (digitToInt)

toList :: String -> [[Int]]
toList = map (map digitToInt) . lines

-- returns if a password contains the login numbers in order.
contains :: [Int] -> [Int] -> Bool
contains _ [] = True
contains [] _ = False
contains (p:ps) login@(l:ls)
	| p == l    = contains ps ls
	| otherwise = contains ps login

main :: IO ()
main = do
	contents <- readFile "79.txt"
	let logins = toList contents
	
	print $ [password | password <- permutations [0,1,2,3,6,7,8,9],
		all (contains password) logins]