{-
How I got the answer:

I was experimenting with the function hyper1777.
I noticed that for k >= 8, (hyper1777 k == 95962097).

Apparently, this trend continues up to 1855.
-}

import Data.List (genericIndex)

-- b ^^ e.
hyperExp :: Integer -> Integer -> Integer
hyperExp b 1 = b
hyperExp b e = b ^ hyperExp b (e - 1)

-- mod8digits !! (n `mod` 1250000) == 1777 ^ n `mod` 10^8
-- length: 1250000
mod8digits :: [Integer]
mod8digits = (1:) $ takeWhile (/= 1) $ iterate (\n -> n * 1777 `mod` 10^8) 1777

-- equal to 1777 ^^ n `mod` 10^8
hyper1777 :: Integer -> Integer
hyper1777 k = iterate (\n -> mod8digits `genericIndex` (n `mod` 1250000)) 1 `genericIndex` k