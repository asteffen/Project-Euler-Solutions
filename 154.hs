import Time

--------------------------------------------------------------------------------
-- Old stuff for testing
--------------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n = product [1..n]

-- multinomial coefficient. a choose bs.
-- http://en.wikipedia.org/wiki/Multinomial_theorem
multinomial :: Integer -> [Integer] -> Integer
multinomial a bs = factorial a `div` product (map factorial bs)

-- all triplets of nonnegative integers that sum to n.
triplets n = [[a, b, c] |
	a <- [0..n],
	b <- [0..n],
	let c = n - a - b,
	c >= 0]

-- 'n'th layer of pascal's pyramid
pyramidLayer :: Integer -> [Integer]
pyramidLayer n = map (multinomial n) $ triplets n

--------------------------------------------------------------------------------
-- New
--------------------------------------------------------------------------------

{-
1

1
1 1

1
2 2
1 2 1

1
3 3
3 6 3
1 3 3 1

1
4 4
6 12 6
4 12 12 4
1 4  6  4 1
-}

-- input: row and column (of pascal's pyramid)
-- output: coordinates of spheres that are above it.
getC :: (Int, Int) -> [(Int, Int)]
getC (y, x) = [(y, x), (y-1,x), (y-1,x-1)]

-- indexes a layer using coordinates
indexLayer :: [[Integer]] -> (Int, Int) -> Integer
indexLayer xs (y, x)
	| x < 0 || y < 0 || x > y || y >= length xs = 0
	| otherwise = xs !! y !! x

-- given one layer of pascal's pyramid, returns the next layer.
nextLayer :: [[Integer]] -> [[Integer]]
nextLayer xs = map getRow [0 .. length xs]
	where
		getElem = sum . map (indexLayer xs) . getC
		getRow y = map (\x -> getElem (y, x)) [0..y]

-- pascal's pyramid represented by an infinite list.
-- each layer is a 2-dimensional list of Integers.
pyramid :: [[[Integer]]]
pyramid = iterate nextLayer [[1]]

-- 'n'th layer of pascal's pyramid
pyramidL = (pyramid !!)

--------------------------------------------------------------------------------
-- Using modulus
--------------------------------------------------------------------------------

{-
variables whose names are appended by 'M' are analogous to those above,
the only difference being that the pyramid is now calculated with a modulus.
-}

modulus :: Integer
modulus = 10^12

nextLayerM :: [[Integer]] -> [[Integer]]
nextLayerM xs = map getRow [0 .. length xs]
	where
		getElem = (`mod` modulus) . sum . map (indexLayer xs) . getC
		getRow y = map (\x -> getElem (y, x)) [0..y]

pyramidM = iterate nextLayerM [[1]]
pyramidLM = (pyramidM !!)

-- returns the number of zeroes in a layer
count0 :: [[Integer]] -> Int
count0 = length . filter (== 0) . concat

--- testing
main = timePrint $ k4 `mod` modulus

ct = length . filter ((== 0) . (`mod` 2)) . concat
cts = map ct pyramid

k1 = product [100001 .. 200000]
k2 = product [1 .. 50000]
k3 = k2 ^ 2
(k4, k5) = k1 `divMod` k3