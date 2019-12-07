-- 34 sec

import Data.List (nub)

-- n choose k.
binom :: Integer -> Integer -> Integer
binom n k = product [n-k+1..n] `div` product [1..k]

-- 4, 9, 16, ...
squares :: [Integer]
squares = map (^2) [2..]

isSquareFree :: Integer -> Bool
isSquareFree n = all ((/= 0) . (n `mod`)) $ takeWhile (<= n) squares

-- first r rows Pascal's triangle
pascalRows :: Integer -> [Integer]
pascalRows r = concatMap (\n -> map (binom n) [0..n]) [0..r-1]

-- sum of distinct squarefree numbers in the first r rows of Pascal's triangle.
sumSqFree :: Integer -> Integer
sumSqFree = sum . filter isSquareFree . nub . pascalRows

main :: IO ()
main = print $ sumSqFree 51