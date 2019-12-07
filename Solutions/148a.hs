pascal = [1] : map nextRow pascal
    where nextRow r = zipWith (\a b -> (a + b) `mod` 7) (0 : r) (r ++ [0])

nnd = map (length . filter (/= 0)) pascal

main = print $ sum $ take (10 ^ 5) nnd