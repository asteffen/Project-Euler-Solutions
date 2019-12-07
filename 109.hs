import Time
import Data.List (sort)

type Mult = Int -- 1, 2, 3
type Score = Int -- 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25
data Throw = Throw Mult Score
	deriving (Eq, Ord)

instance Show Throw where
	show (Throw m s) = (: show s) $ case m of
		1 -> 'S'; 2 -> 'D'; 3 -> 'T'

value :: Throw -> Int
value (Throw m s) = m * s

singles, doubles, triples :: [Throw]
singles = map (Throw 1) $ [1..20] ++ [25]
doubles = map (Throw 2) $ [1..20] ++ [25]
triples = map (Throw 3) $ [1..20]

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

-- List of every distinct way to get a score of (n) using 2 throws or less.
ways2throws :: Int -> [[Throw]]
ways2throws 0 = [[]]
ways2throws n = filter isValid $ concatMap (\a -> map (a :) $ cache !! (n - value a)) avail
	where
		avail = concatMap (takeWhile ((<= n) . value)) [singles, doubles, triples]
		isValid xs = length xs <= 2 && isSorted xs

-- Used for memoization.
cache :: [[[Throw]]]
cache = map ways2throws [0..]

-- All ways to check out with a score of (n). Makes sure a double is the last throw.
waysCheckOut :: Int -> [[Throw]]
waysCheckOut n = concatMap (\a -> map (a :) $ ways2throws $ n - value a) avail
	where avail = takeWhile ((<= n) . value) doubles

main :: IO ()
main = timePrint $ sum $ map (length . waysCheckOut) [1..99]

--------------------------------------------------------------------------------
-- Alternate method (simpler and faster)
--------------------------------------------------------------------------------

sVals, dVals, tVals, allVals :: [Int]
sVals = [1..20] ++ [25]
dVals = map (* 2) sVals
tVals = map (* 3) [1..20]
allVals = sort $ 0 : sVals ++ dVals ++ tVals

allSetsLess100 :: [(Int, Int, Int)]
allSetsLess100 = [(throw1, throw2, throw3) |
	throw1 <- dVals,
	index2 <- [0 .. maxIndex],
	index3 <- [index2 .. maxIndex],
	let throw2 = allVals !! index2,
	let throw3 = allVals !! index3,
	throw1 + throw2 + throw3 < 100]
	where maxIndex = length allVals - 1

ans :: Int
ans = length allSetsLess100