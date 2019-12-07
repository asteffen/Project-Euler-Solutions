import System.IO.Unsafe (unsafePerformIO)

smat =
    [[131, 673, 234, 103, 18]
    ,[201, 96, 342, 965, 150]
    ,[630, 803, 746, 422, 111]
    ,[537, 699, 497, 121, 956]
    ,[805, 732, 524, 37, 331]]

toMatrix :: String -> [[Integer]]
toMatrix = map toList . lines
	where toList str = read $ "[" ++ str ++ "]"

mat ! (y, x) = mat !! y !! x

startpt mat (y, x)
    | x == length mat - 1 = mat ! (y, x)
    | otherwise = minimum $ map chooserow [0 .. length mat - 1]
    where
        -- minimum path when first traveling vertically to row (y1).
        chooserow y1 = startpt mat (y1, x + 1) + sum (map (\b -> mat ! (b, x)) vert_vals)
            where vert_vals = if y1 > y then [y..y1] else [y1..y]

startpt_memo mat (y, x)
    | x == length mat - 1 = mat ! (y, x)
    | otherwise = minimum $ map chooserow [0 .. length mat - 1]
    where
        -- minimum path when first traveling vertically to row (y1).
        chooserow y1 = memo_mat ! (y1, x + 1) + sum (map (\b -> mat ! (b, x)) vert_vals)
            where vert_vals = if y1 > y then [y..y1] else [y1..y]

memo_mat = map (\y -> map (\x -> startpt_memo mat (y, x)) [0..79]) [0..79]

mat = toMatrix $ unsafePerformIO $ readFile "82.txt"

main = print $ minimum $ [startpt_memo mat (y, 0) | y <- [0..79]]