import Data.Char (intToDigit)

f n d = length $ filter (== intToDigit d) $ concatMap show [0..n]

num1s = map (length . filter (== '1') . show) [0..]
cumulNum1s = scanl1 (+) num1s

-- diffs !! n = n - f(n, 1)
diffs = zipWith (-) [0..] cumulNum1s

zipDiffs = zip [0..] diffs

sols = map fst $ filter ((== 0) . snd) zipDiffs