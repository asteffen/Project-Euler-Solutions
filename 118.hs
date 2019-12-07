-- 425 sec, compiled -O

{-
-- used to make 118.txt
write :: [Integer] -> IO ()
write xs = do
	appendFile "118.txt" $ unlines $ map show first1000
	putStrLn $ "Wrote up to " ++ show (last first1000) -- up to, but not including
	write rest
	where (first1000, rest) = splitAt 1000 xs
-}

import Data.Numbers.Primes (primes)
import System.IO.Unsafe (unsafePerformIO)
import Data.List ((\\))
import Time

-- Returns (True) if the list contains no duplicate elements.
uniq :: (Eq a) => [a] -> Bool
uniq [] = True
uniq (x:xs) = all (/= x) xs && uniq xs

-- Returns (True) if the Integer does not contain 0 or duplicate digits.
qualifies :: Integer -> Bool
qualifies n = '0' `notElem` str && uniq str
	where str = show n

{-
A list of all primes in the range (1, 10^8) that satisfy (qualifies).

No 9-digit primes satisfy (qualifies) because they would have to contain each
digit besides 0, and would be divisible by 9 because the sum of the digits is 45.
-}
qualPrimes :: [String]
qualPrimes = lines $ unsafePerformIO $ readFile "118.txt"

-- Returns (True) if every element in (xs) is in (ys).
isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset xs ys = all (`elem` ys) xs

-- The number of ways to express (y) as the union of disjoint lists from (xs).
waysToMake :: (Eq a) => [a] -> [[a]] -> Integer
waysToMake [] _ = 1
waysToMake _ [] = 0
waysToMake y (x:xs)
	| isSubset x y = waysToMake (y \\ x) xs + rest
	| otherwise    = rest
	where rest = waysToMake y xs

main :: IO ()
main = timePrint $ waysToMake "123456789" qualPrimes