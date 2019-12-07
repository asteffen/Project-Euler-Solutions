binom n r = product [n-r+1..n] `div` product [1..r]

grtrMillion = [b | n <- [1..100], r <- [0..n], let b = binom n r,
	b > 1000000]

main = print $ length $ grtrMillion