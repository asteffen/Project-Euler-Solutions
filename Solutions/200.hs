import Data.Numbers.Primes (primes)
import Data.List (sort)
import Data.Function (on)
import Euler (isPrime)
import Time

-- infinite 2-dimensional list of squbes
squbes :: [[Integer]]
squbes = [[p * p * q * q * q | q <- primes, q /= p] | p <- primes]

-- apply function (f) on the (n)th element of (xs)
apply :: (a -> a) -> Int -> [a] -> [a]
apply _ _ [] = []
apply f 0 (x:xs) = f x : xs
apply f n (x:xs) = x : apply f (n - 1) xs

contains200 :: String -> Bool
contains200 ('2':'0':'0':_) = True
contains200 [] = False
contains200 str = contains200 $ tail str

-- all possible combinations from changing 1 digit in the string
change1 :: String -> [String]
change1 str = [apply (const d) i str | i <- [0 .. length str - 1],
	d <- ['0'..'9'], d /= str !! i]

primeProof :: String -> Bool
primeProof = not . any (isPrime . read) . change1

qual :: Integer -> Bool
qual n = contains200 n' && primeProof n'
	where n' = show n

squbesBelow :: Integer -> [Integer]
squbesBelow n = concat $ takeWhile (not . null) $ map (takeWhile (< n)) squbes

main = timePrint $ (!! 199) $ sort $ filter qual $ squbesBelow $ 10^12

-- the way i sorted these infinite 2-dimensional lists was cool, but it turns
-- out they were unnecessary.
{-
-- increments the (row)th element of (cols), then appends a 0 if the last
-- element is now 1.
update :: [Int] -> Int -> [Int]
update cols row
	| row == length cols - 1 = cols' ++ [0]
	| otherwise = cols'
	where cols' = apply (+1) row cols

-- recursive function used to generate (sortedSqubes).
-- (cols) keeps track of which column to compare in each row.
-- e.g. (cols !! 3) = lowest column in row 3 that is not used yet.
flattenSort :: [[Integer]] -> [Int] -> [Integer]
flattenSort grid cols = nElem : flattenSort grid (update cols nRow) where
	candidates = zipWith (\row col -> (row, grid !! row !! col)) [0..] cols
	(nRow, nElem) = minimumBy (compare `on` snd) candidates

-- infinite list of squbes, sorted
sortedSqubes :: [Integer]
sortedSqubes = flattenSort squbes [0, 0]

digitStrs = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

incDigits :: [[String]]
incDigits = digitStrs : [[a ++ b | a <- strs, b <- digitStrs] | strs <- incDigits]

suffixes = "" : concat incDigits

c200 :: [[Integer]]
c200 = [[read $ show a ++ "200" ++ b | b <- suffixes] | a <- [0..]]

c200s = flattenSort c200 [0]

isSqube n = exps == [2, 3] || exps == [3, 2]
	where exps = map length $ group $ factorTD n
-}