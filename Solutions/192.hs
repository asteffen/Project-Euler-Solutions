import Data.Ratio

eval [n] = n
eval (n:ns) = n + 1 / eval ns

rt n x = x : rt n ((n/x + x) / 2)

rt' n = rt (toRational n) (toRational $ sqrt n) !! 10

cf x
	| y == 0 = [n]
	| otherwise = n : cf (1 / y)
	where
		n = floor x
		y = x - fromIntegral n