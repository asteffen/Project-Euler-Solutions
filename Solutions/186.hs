import qualified Data.IntSet as S
import Time
import Data.List
import System.IO.Unsafe

lagfib :: [Int]
lagfib = map (`mod` 1000000) $ map (\k -> 100003 - 200003*k + 300007*k*k*k) [1..55]
	++ zipWith (+) lagfib (drop 31 lagfib)

pairs :: [[Int]]
pairs = f lagfib
	where f (a:b:rest)
		| a == b = f rest
		| otherwise = [a, b] : f rest

findDelete _ [] = Nothing
findDelete p (y:ys)
	| p y = Just (y, ys)
	| otherwise = case findDelete p ys of
		Nothing -> Nothing
		Just (y', ys') -> Just (y', y : ys')

-- faster
addf :: [S.IntSet] -> [Int] -> [S.IntSet]
addf grps0 [a, b] = case findDelete (a `S.member`) grps0 of
	Just (ga, grps1) -> case findDelete (b `S.member`) grps1 of
		Just (gb, grps2) -> (ga `S.union` gb) : grps2
		Nothing -> (S.insert b ga) : grps1
	Nothing -> case findDelete (b `S.member`) grps0 of
		Just (gb, grps2) -> (S.insert a gb) : grps2
		Nothing -> S.fromList [a, b] : grps0

gg = foldl addf [] $ take (180 * 1000) pairs

-- (x, count)
countFreq :: (Ord a) => [a] -> [(a, Int)]
countFreq = map (\xs -> (head xs, length xs)) . group . sort

main = do
	timePrint $ length gg
	print $ countFreq $ map S.size gg

{-
I messed up a bit.
I forgot to handle miscalls, i.e. when the two numbers in the pair are the same.

the first miscall is pairs !! 851371.

-- using addp

format = pairs, groups(merged), time(sec)
6000 5933 5
10000 9808 15 - [(2,9617),(3,190),(4,1)]
25000 23805 90

50000 45205 359
[(2,41035),(3,3634),(4,462),(5,61),(6,11),(7,2)]

120000 93423 2048
[(2,74353),(3,13963),(4,3532),(5,1056),(6,330),(7,118),(8,43),(9,16),(10,8),(11,2),(12,2)]

130000 99006 2399
[(2,77345),(3,15489),(4,4167),(5,1300),(6,438),(7,156),(8,70),(9,23),(10,9),(11,5),(12,3),(13,1)]

added 50000 more pairs
180000 122506 1821
[(2,87757),(3,22101),(4,7342),(5,2925),(6,1212),(7,577),(8,278),(9,157),(10,85),(11,29),(12,17),(13,
12),(14,7),(15,3),(17,1),(18,1),(20,1),(21,1)]

added 30k more pairs
210k 133030 1261
[(2,90673),(3,25082),(4,9236),(5,3943),(6,1924),(7,968),(8,499),(9,289),(10,155),(11,102),(12,59),(1
3,38),(14,23),(15,14),(16,6),(17,7),(18,4),(19,1),(20,3),(21,1),(23,1),(24,1),(26,1)]

added 80k
290k 150013 4230
[(2,90646),(3,29635),(4,12801),(5,6628),(6,3590),(7,2218),(8,1361),(9,902),(10,583),(11,425),(12,288
),(13,216),(14,180),(15,114),(16,98),(17,75),(18,52),(19,49),(20,29),(21,25),(22,22),(23,13),(24,10)
,(25,10),(26,9),(27,5),(28,6),(29,2),(31,5),(32,2),(33,4),(34,2),(35,1),(36,3),(37,2),(40,1),(42,1)]

-----------
using addf
new: lagfib instead of fibgen, much faster, uses zipwith.
uses sets instead of lists
uses findDelete
does not count duplicates (miscalls)

180k
122506
Computation time = 2256.062500 sec
[(2,87757),(3,22101),(4,7342),(5,2925),(6,1212),(7,577),(8,278),(9,157),(10,85),(11,29),(12,17),(13,
12),(14,7),(15,3),(17,1),(18,1),(20,1),(21,1)]



-}

