{-
Expression:
timePrint $ length $ traverseTree $ makeTree [2,2]
(compiled -O)

Result:
1277025750
Computation time = 3321.125000 sec

So there are over 1.2 billion ways to choose the papers.
The brute-force probability method is probably infeasible because the
computer will run out of memory.
-}

import System.Random (randomRIO)
import Data.List (delete, genericLength)
import Control.Monad (replicateM)
import Time
import Data.Ratio

--------------------------------------------------------------------------------
-- Using simulation (too slow, not accurate enough)
--------------------------------------------------------------------------------

-- returns a random element in the given list.
randomElem :: [a] -> IO a
randomElem xs = do
	i <- randomRIO (0, length xs - 1)
	return $ xs !! i

-- removes a random paper. if it is A5, removes it.
-- else, keeps on cutting it in half until there is an A5, and removes it.
-- the Bool value in the return specifies whether an A5 sheet was removed.
getPaper :: [Int] -> IO [Int]
getPaper ps = do
	p <- randomElem ps
	let rest = delete p ps
	if p == 5
		then return rest
		else return $ rest ++ [p + 1 .. 5]

iterateCut :: [Int] -> IO [[Int]]
iterateCut [] = return []
iterateCut ps = do
	ps' <- getPaper ps
	rest <- iterateCut ps'
	return $ ps : rest

-- the number of times the foreman finds a single sheet of paper
-- excludes first and last batches.
numSingles :: IO Int
numSingles = do
	pss <- iterateCut [1]
	return $ length $ filter ((== 1) . length) $ init $ tail pss

-- performs (numSingles) (trials) times and returns the expected number of times
-- the foreman finds a single sheet of paper in the envelope.
simulate :: Int -> IO Double
simulate trials = do
	ns <- replicateM trials numSingles
	return $ fromIntegral (sum ns) / fromIntegral trials

{-
main :: IO ()
main = do
	a <- timeIO $ simulate $ 10 ^ 6
	print a
-}

--------------------------------------------------------------------------------
-- Using probability
--------------------------------------------------------------------------------

-- returns a list of all possible outcomes by choosing 1 paper
getPaperList :: [Int] -> [[Int]]
getPaperList ps = do
	p <- ps
	let rest = delete p ps
	if p == 5
		then return rest
		else return $ rest ++ [p + 1 .. 5]

data Tree a = Node a [Tree a]
	deriving (Show, Read)

-- returns a list of all paths possible from the root node to the leaf nodes.
traverseTree :: Tree a -> [[a]]
traverseTree (Node a []) = [[a]]
traverseTree (Node a xs) = map (a :) $ concatMap traverseTree xs

-- given a list of paper sizes, makes a tree where each branch corresponds
-- to the paper chosen by the foreman to cut.
makeTree :: [Int] -> Tree [Int]
makeTree ps
	| ps == [5] = Node [5] []
	| otherwise = Node ps $ map makeTree $ getPaperList ps

-- returns the probability that a given path is taken
probability :: [[Int]] -> Rational
probability = (1 %) . product . map genericLength

-- used to calculate expected value given list of paths.
calcExp :: [[[Int]]] -> Rational
calcExp = foldl (\acc path -> acc + probability path * (genericLength $
	filter ((== 1) . length) $ init $ tail path)) 0

-- given a list of initial paper sizes, returns the expected number of times
-- that a single paper is encountered. does not count first/last.
expected :: [Int] -> Rational
expected = calcExp . traverseTree . makeTree
{-
main = do
	a <- readFile "151.txt"
	let b = read a :: Tree [Int]
	let c = traverseTree b :: [[[Int]]]
	timeIO $ mapM_ (appendFile "151t.txt" . (++ "\n") . show)
-}