import Data.List (intercalate)
import Time

type Grid = [[Int]]

digits :: [Int]
digits = [0..9]

isDigit :: Int -> Bool
isDigit n = 0 <= n && n <= 9

-- ugly brute force.
-- returns all the grids which have the given row sum for all rows, columns, and diagonals.
ways :: Int -> [Grid]
ways rowSum = [
	[[a, b, c, d]
	,[e, f, g, h]
	,[i, j, k, l]
	,[m, n, o, p]] |
	
	-- first row
	a <- takeWhile (<= rowSum) digits,
	b <- takeWhile (<= rowSum - a) digits,
	c <- takeWhile (<= rowSum - a - b) digits,
	let d = rowSum - a - b - c, isDigit d,
	
	-- second row
	e <- takeWhile (<= rowSum) digits,
	f <- takeWhile (<= rowSum - e) digits,
	g <- takeWhile (<= rowSum - e - f) digits,
	let h = rowSum - e - f - g, isDigit h,
	
	-- lower left quadrant
	i <- takeWhile (<= rowSum) digits,
	let m = rowSum - a - e - i, isDigit m,
	let j = rowSum - d - g - m, isDigit j,
	let n = rowSum - b - f - j, isDigit n,
	
	-- lower right quadrant
	k <- takeWhile (<= rowSum) digits,
	let o = rowSum - c - g - k, isDigit o,
	let l = rowSum - i - j - k, isDigit l,
	let p = rowSum - m - n - o, isDigit p,
	
	-- sums that were not checked already
	d + h + l + p == rowSum,
	a + f + k + p == rowSum
	]

main :: IO ()
main = timePrint $ sum $ map (length . ways) [0..36]

--------------------------------------------------------------------------------
-- Functions for testing
--------------------------------------------------------------------------------

printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . intercalate " " . map show)

testGrid :: Grid
testGrid =
	[[6,3,3,0]
	,[5,0,4,3]
	,[0,7,1,4]
	,[1,2,4,5]]

isValid :: Int -> Grid -> Bool
isValid rowSum
	[[a, b, c, d]
	,[e, f, g, h]
	,[i, j, k, l]
	,[m, n, o, p]]
	= all ((== rowSum) . sum)
		[[a, b, c, d]
		,[e, f, g, h]
		,[i, j, k, l]
		,[m, n, o, p]
		,[a, e, i, m]
		,[b, f, j, n]
		,[c, g, k, o]
		,[d, h, l, p]
		,[a, f, k, p]
		,[d, g, j, m]]