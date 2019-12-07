{-
986262
Computation time = 148.031250 sec
-}

import Euler (factorTD)
import Data.List (sort, group)
--import Time

numDivisors :: Integer -> Int
numDivisors = product . map ((+1) . length) . group . factorTD

divisorsList :: [Int]
divisorsList = map numDivisors [1..10^7]

main :: IO ()
--main = timePrint $ length $ filter id $ zipWith (==) divisorsList (tail divisorsList)
main = print $ length $ filter id $ zipWith (==) divisorsList (tail divisorsList)
