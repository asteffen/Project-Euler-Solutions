{-
Looking back, it probably would have been easier to generate the sums of
consecutive squares first and then check for palindromes...

compiled -O:

2906969179
Computation time = 231.359375 sec
-}

import Data.List (tails)
import Time

-- palindromes !! n = list of all n-digit palindromes, including numbers
-- starting with 0.
palindromeStrs :: [[String]]
palindromeStrs = [""] : map show [0..9] : map f palindromeStrs
	where f ps = [[d] ++ str ++ [d] | d <- "0123456789", str <- ps]

-- infinite list of palindromes
palindromes :: [Integer]
palindromes = map read $ filter ((/= '0') . head) $ tail $ concat palindromeStrs

-- sumConsecSq !! n = partial sums of {(n+1)^2, (n+2)^2, ...}
sumConsecSq :: [[Integer]]
sumConsecSq = map (scanl1 (+)) $ tails $ map (^ 2) [1..]

-- Returns whether (n) can be expressed as the sum of consecutive squares.
isSumConsecSq :: Integer -> Bool
isSumConsecSq n = (n `elem`) $ map last $ takeWhile ((> 1) . length) $
	map (takeWhile (<= n)) sumConsecSq

main :: IO ()
main = timePrint $ sum $ filter isSumConsecSq $ takeWhile (< 10^8) palindromes