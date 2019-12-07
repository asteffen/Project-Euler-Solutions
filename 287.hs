import Time

type Image = [[Int]]
{-
data Color = Black | White
	deriving (Eq)
-}
black = 0
white = 1

-- D_n.
dn :: Int -> Image
dn n = map (\y -> map (getColor y) rng) $ reverse rng
	where
		rng = [0 .. 2 ^ n - 1]
		a = 2 ^ (n - 1)
		getColor y x = if (x - a) ^ 2 + (y - a) ^ 2 <= a ^ 2
			then black
			else white

p = printImage
printImage :: Image -> IO ()
printImage = mapM_ $ \xs -> mapM_ (putStr . show') xs >> putStrLn ""
	where
		show' x = if x == black then "#" else "_"

-- splits image into  4 quadrants
split :: Image -> [Image]
split img = [topLeft, topRight, bottomLeft, bottomRight]
	where
		dim = length img `div` 2
		(left, right) = unzip $ map (splitAt dim) img
		(topLeft, bottomLeft) = splitAt dim left
		(topRight, bottomRight) = splitAt dim right

-- returns minimal encoding of image
encode :: Image -> String
encode img
	| all (== black) pixels = "10"
	| all (== white) pixels = "11"
	| otherwise = "0" ++ concat (map encode $ split img)
	where pixels = concat img

sample :: Image
sample = map (map (\x -> if x == 0 then black else white))
	[[0,0,0,1]
	,[0,0,1,0]
	,[1,1,0,0]
	,[1,1,0,0]]

main = timePrint $ (length . encode . dn) 11