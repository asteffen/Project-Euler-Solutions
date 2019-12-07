num = sum [n^n | n <- [1..1000]]

main = print $ num `mod` (10^10)