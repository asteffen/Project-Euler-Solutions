fibs :: [Integer]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs) ]

main :: IO ()
main = putStrLn $ show $ sum $ filter even $ takeWhile (<4000000) fibs

-- function composition form:
-- main = (putStrLn . show . sum . (filter even) . (takeWhile (<4000000))) fibs