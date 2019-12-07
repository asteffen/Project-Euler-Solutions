-- http://en.wikipedia.org/wiki/Prim%27s_algorithm

import System.IO.Unsafe (unsafePerformIO)
import Data.List (minimumBy)
import Data.Function (on)

type Vertex = Int
type Edge = (Vertex, Vertex)
type Weight = Integer

-- Index lists starting at 1.
(!) :: [a] -> Int -> a
xs ! i = xs !! (i - 1)

replaceAll :: (Eq a) => a -> a -> [a] -> [a]
replaceAll old new = foldr (\x acc -> if x == old then new : acc else x : acc) []

graph :: [[Weight]]
graph = map (\line -> read $ "[" ++ line ++ "]") $ lines $ replaceAll '-' '0' $
	unsafePerformIO $ readFile "107.txt"

labelIndices :: [[Weight]] -> [[(Edge, Weight)]]
labelIndices = zipWith (\y row -> zip (map ((,) y) [1..]) row) [1..]

minimumSpanningTree :: [[Weight]] -> [(Edge, Weight)]
minimumSpanningTree adjMatrix = mst [1] []
	where
		labeledMatrix = labelIndices adjMatrix
		numVertices = length adjMatrix
		
		mst :: [Vertex] -> [(Edge, Weight)] -> [(Edge, Weight)]
		mst vertices ret
			| length vertices == numVertices = ret
			| otherwise = mst (newV : vertices) (newAssoc : ret)
			where
				qualifies ((_, v2), w) = v2 `notElem` vertices && w /= 0
				possibleV = filter qualifies $ concatMap (labeledMatrix !) vertices
				newAssoc@((_, newV), _) = minimumBy (compare `on` snd) possibleV

main :: IO ()
main = print $ oldWeight - newWeight
	where
		oldWeight = (`div` 2) $ sum $ concat $ graph
		newWeight = sum $ map snd $ minimumSpanningTree graph