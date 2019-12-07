fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

numRed :: Int -> Integer
numRed n = fibs !! (n + 1) - 1

greens :: [Integer]
greens = 0 : 0 : 1 : zipWith (+) greens (drop 2 greens)

numGreen :: Int -> Integer
numGreen n = greens !! (n + 2) - 1

blues :: [Integer]
blues = 0 : 0 : 0 : 1 : zipWith (+) blues (drop 3 blues)

numBlue :: Int -> Integer
numBlue n = blues !! (n + 3) - 1

numWays :: Int -> Integer
numWays n = numRed n + numGreen n + numBlue n

main :: IO ()
main = print $ numWays 50