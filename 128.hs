{-
hexagonal coordinates
   _
 _/y\_
/z\_/x\
\_/0\_/
/ \_/ \
\_/ \_/
  \_/

3 coordinates for each hexagon, 1 is redundant

(vectors)
z = y - x
y = x + z
x = y - z
-}

import Data.Array
import Data.List
import Euler
import Time

type Hex = (Integer, Integer)

-- list of (x, y) vectors in each of the 6 directions.
-- describes the ring: 2, 3, 4, 5, 6, 7, 2
aroundMoves :: [Hex]
aroundMoves = [(-1,0),(0,-1),(1,-1),(1,0),(0,1),(-1,1)]

addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- returns list of coordinates in a ring, starting from the top and moving counterclockwise
getRing r = init $ scanl addCoords (0, r) $ concatMap (genericReplicate r) aroundMoves

-- list of all coordinates in order
hexs = (0, 0) : concatMap getRing [1..]

-- ((x, y), num)
hexNums = zip hexs [1..5000000]

-- array for efficiency
grid = accumArray (\a b -> b) 0 ((-mx, -mx), (mx, mx)) hexNums
mx = (+1) $ maximum $ map (snd . fst) hexNums

-- given a hexNum pair, return True if PD(n) = 3
isPD (hex, n) = (== 3) $ length $ filter id $
	map (isPrimeTD . abs . (n -) .(grid !) . addCoords hex) aroundMoves

-- list of numbers that satisfy PD
pds = filter isPD hexNums

--I noticed all in PDs have an x coordinate of 0 or 1... optimize.
pds2 = filter isPD $ filter (\((x,y),_) -> x == 0 || x == 1) hexNums

main = do
	print mx
	timePrint $ length pds2
	print $ last pds2