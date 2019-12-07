-- Binomial coefficients.
binom :: (Integral a) => a -> a -> a
binom n k = product [n-k+1..n] `div` product [1..k]

-- number of ways to go from the top left corner in an (n) by (n) grid to the
-- bottom right corner without backtracking
num_paths :: (Integral a) => a -> a
num_paths n = binom (2*n) n

main = putStrLn $ show $ num_paths 20