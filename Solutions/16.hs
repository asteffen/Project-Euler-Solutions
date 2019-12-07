num :: Integer
num = 2 ^ 1000

ans :: Integer
ans = sum $ map charToInteger $ show num
	where charToInteger c = read [c] :: Integer

main = putStrLn $ show $ ans