-- Fibonacci numbers, as an infinite list
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

zfibs :: [(Integer, Integer)]
zfibs = zip [0..] fibs

num_digits :: Integer -> Int
num_digits = length . show

{-
I'm not sure if this is faster, but it fails on large numbers.

num_digits2 :: Integer -> Integer
num_digits2 = (1+) . floor . logBase 10 . fromIntegral
-}

main :: IO ()
main = print $ head $ filter (\(_,n) -> num_digits n >= 1000) zfibs