import Euler (isPrimeTD)
import Data.List -- (genericLength)
import Time

nthRow n = [a .. a + n - 1]
	where a = (n*n-n+2) `div` 2

getElem :: Integer -> Integer -> Integer
getElem row n
	| n < 0 || n >= genericLength nrow = 0
	| otherwise = genericIndex nrow n
	where nrow = nthRow row

nbrs row n = [(r, i, getElem r i) | r <- rows, i <- indices, r /= row || i /= n]
	where
		rows = [row - 1 .. row + 1]
		indices = [n - 1 .. n + 1]

thr (_, _, a) = a

isInTrip row n
	| not $ isPrimeTD $ getElem row n = False
	| len == 0 = False
	| len >= 2 = True
	| len == 1 = any (>= 2) [length $ filter isPrimeTD $ map thr $ nbrs a b | (a, b, _) <- pns]
	where
		ns = nbrs row n
		pns = filter (isPrimeTD . thr) ns
		len = length pns

sFunc row = sum $ map (getElem row) $ filter (isInTrip row) [0 .. row - 1]

main = timePrint $ sFunc 10000