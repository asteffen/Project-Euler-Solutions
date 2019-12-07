import Data.Bits (testBit, bitSize)
import Data.List (sort, foldl')

-- taken from
-- http://www.haskell.org/haskellwiki/The_Fibonacci_sequence#Fastest_Fib_in_the_West
fib :: Int -> Integer
fib n = snd . foldl' fib' (1, 0) $ dropWhile not $
            [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib' (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g

modulus :: Integer
modulus = 10 ^ 9

-- The fibonacci sequence modulo (modulus).
fibsMod :: [Integer]
fibsMod = 0 : 1 : zipWith (\a b -> (a + b) `mod` modulus) fibsMod (tail fibsMod)

-- Pairs of the form (n, F_n).
numberedFibsMod :: [(Integer, Integer)]
numberedFibsMod = zip [0..] fibsMod

-- The indices (n) such that the last 9 digits of (F_n) are 1-9 pandigital.
indicesLastPandig :: [Integer]
indicesLastPandig = map fst $ filter ((== "123456789") . sort . show . snd) numberedFibsMod

-- Returns (True) if the first 9 digits of the given Integer are 1-9 pandigital.
first9pandig :: Integer -> Bool
first9pandig = (== "123456789") . sort . take 9 . show

main :: IO ()
main = print $ head $ filter (first9pandig . fib . fromIntegral) indicesLastPandig