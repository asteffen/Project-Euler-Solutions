{-
h2 + (b/2)2 = L2
h2 + b2/4 = L2
4h2 + b2 = 4L2

case 1. h = b - 1
4(b - 1)2 + b2 = 4L2
4(b2 - 2b + 1) + b2 = 4L2
4b2 - 8b + 4 + b2 = 4L2
5b2 - 8b + 4 - 4L2 = 0

case 2. h = b + 1
4(b + 1)2 + b2 = 4L2
4(b2 + 2b + 1) + b2 = 4L2
4b2 + 8b + 4 + b2 = 4L2
5b2 + 8b + 4 - 4L2 = 0
-}

import Data.List

-- found solutions using http://www.alpertron.com.ar/QUAD.HTM
case1sols = iterate (\(x, y) -> (-9*x - 8*y + 8, -10*x - 9*y + 8)) (0, 1)
case2sols = iterate (\(x, y) -> (-9*x - 8*y - 8, -10*x - 9*y - 8)) (0, -1)
sols = drop 2 $ filter (>0) $ map snd $ concat $ transpose [case1sols, case2sols]

main = print $ sum $ take 12 sols