u :: [Double]
u = (-1) : [fromIntegral (floor (2 ** (30.403243784 - x * x))) / 10 ^ 9 |
	n <- [0..], let x = u !! n]

-- uSums converges pretty quickly.
-- for all n >= 256, u_n = 1.710637717.
uSums :: [Double]
uSums = [u !! n + u !! (n + 1) | n <- [0,2..]]

main :: IO ()
main = print $ uSums !! 256