{-
I didn't try this program over all numbers in the range [1..1000000].
At first I tried it, but it got a stack overflow.
So I split up the range into 10 smaller blocks, each of size 100000.
I started trying each block in descending order:

range                output
[900000..1000000] -> (939497, 507)
[800000..900000]  -> (837799, 525)
[700000..800000]  -> (704623, 504)

Each sub-block took about 90 seconds to calculate.
I didn't try any others, becuase I found the answer was 837799.
-}

range = [800000..900000]

collatz n
	| even n    = n `div` 2
	| otherwise = 3*n + 1

collatzChain 1 = [1]
collatzChain n = n : collatzChain (collatz n)

collatz_lengths = map (\n -> (n, length $ collatzChain n)) [700000..800000]

max_starting = foldl1 fold_func collatz_lengths
	where fold_func acc@(acc_n, acc_len) x@(x_n, x_len)
		| x_len > acc_len = x
		| otherwise       = acc

main = putStrLn $ show $ max_starting

----- experimental

-- col !! n = length of collatz chain starting with n
-- col !! 0 = -1, a placeholder.
col = (-1) : 1 : [ col !! (collatz n) + 1 | n <- [2..]]

cc = (-1) : map (length.collatzChain) [1..]