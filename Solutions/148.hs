--------------------------------------------------------------------------------
-- Brute-force functions (for verifying optimized functions)
--------------------------------------------------------------------------------

-- (n) choose (k).
binom :: Integer -> Integer -> Integer
binom n k = product [n - k + 1 .. n] `div` product [1 .. k]

-- Row (n) of Pascal's triangle.
pascalRow :: Integer -> [Integer]
pascalRow n = map (binom n) [0..n]

-- (True) if a number is not divisible by 7.
notDiv7 :: Integer -> Bool
notDiv7 = (/=0) . (`mod` 7)

-- The number of entries in row (n) of Pascal's triangle that are not divisible
-- by 7.
numNotDiv7 :: Integer -> Int
numNotDiv7 = length . filter notDiv7 . pascalRow

-- The number of entries in the first (n) rows of Pascal's triangle that are
-- not divisible by 7.
totalNotDiv7 :: Integer -> Int
totalNotDiv7 n = sum $ map numNotDiv7 [0 .. n - 1]

--------------------------------------------------------------------------------
-- Optimized functions
--------------------------------------------------------------------------------

-- block 1 (indices 0 to 48) of the pattern.
block1 :: [Integer]
block1 = [a * b | a <- [1..7], b <- [1..7]]

-- the nth block in the pattern
block :: Integer -> [Integer]
block n = map (*n) block1

-- list of values where the nth element is the number of entries in row n of
-- pascal's triangle not divisible by 7.
numNotDiv7list :: [Integer]
numNotDiv7list = concatMap block [1..]

-- much faster version of totalNotDiv7 using list above.
totalNotDiv7_2 :: Int -> Integer
totalNotDiv7_2 n = sum $ take n numNotDiv7list

totalNotDiv7_3 :: Integer -> Integer
totalNotDiv7_3 n
	| mod49 == 0 = div49 * (div49 + 1) `div` 2 * 784
	| otherwise  = totalNotDiv7_3 (n - mod49) + leftover
	where
		(div49, mod49) = n `divMod` 49
		leftover = sum $ take (fromIntegral mod49) $ block $ div49 + 1

-- alternate version of (mod).
-- if a `mod` b == 0, then it returns (b) instead of 0.
a `mod2` b = if m == 0 then b else m
	where m = a `mod` b

-- alternate version of (div).
-- rounds up instead of down.
a `div2` b = ceiling (fromIntegral a / fromIntegral b)

{- slower than above.
a `div2` b = if m == 0 then d else d + 1
	where (d, m) = a `divMod` b
-}

-- efficient version of numNotDiv7
numNotDiv7_2 :: Integer -> Integer
numNotDiv7_2 n
	| power7 == 7 = n
	| otherwise = n `div2` lessPower7 * numNotDiv7_2 (n `mod2` lessPower7)
	where
		-- the lowest power of 7 greater than (n).
		power7 = until (>=n) (*7) 7
		
		lessPower7 = power7 `div` 7

-- converts an Integer its base 7 representation (as a list).
toBase7 n | n <= 7 = [n]
toBase7 n = d : toBase7 m
	where (d, m) = n `divMod` 7

{-
map numNotDiv7 [0..] =

index 0
1,2,3,4,5,6,7,
2,4,6,8,10,12,14,
3,6,9,12,15,18,21,
4,8,12,16,20,24,28,
5,10,15,20,25,30,35,
6,12,18,24,30,36,42,
7,14,21,28,35,42,49,

index 49
2,4,6,8,10,12,14,
4,8,12,16,20,24,28,
6,12,18,24,30,36,42,
8,16,24,32,40,48,56,
10,20,30,40,50,60,70,
12,24,36,48,60,72,84,
14,28,42,56,70,84,98,

index 98
3,6,9,12,15,18,21,
6,12,18,24,30,36,42,
9,18,27,36,45,54,63,
12,24,36,48,60,72,84,
15,30,45,60,75,90,105,
18,36,54,72,90,108,126,
21,42,63,84,105,126,147,

index 147
4,8,12,16,20,24,28,
8,16,24,32,40,48,56,
12,24,36,48,60,72,84,
16,32,48,64,80,96,112,
20,40,60,80,100,120,140,
24,48,72,96,120,144,168,
28,56,84,112,140,168,196,

index 196
5,10,15,20,25,30,35,
10,20,30,40,50,60,70,
15,30,45,60,75,90,105,
20,40,60,80,100,120,140,
25,50,75,100,125,150,175,
30,60,90,120,150,180,210,
35,70,105,140,175,210,245,

index 245
6,12,18,24,30,36,42,
12,24,36,48,60,72,84,
18,36,54,72,90,108,126,
24,48,72,96,120,144,168,
30,60,90,120,150,180,210,
36,72,108,144,180,216,252,
42,84,126,168,210,252,294,

index 294
7,14,21,28,35,42,49,
14,28,42,56,70,84,98,
21,42,63,84,105,126,147,
28,56,84,112,140,168,196,
35,70,105,140,175,210,245,
42,84,126,168,210,252,294,
49,98,147,196,245,294,343,

index 343 (reset)
2,4,6,8,10,12,14,
4,8,12,16,20,24,28,
6,12,18,24,30,36,42,
8,16,24,32,40,48,56,
10,20,30,40,50,60,70,
12,24,36,48,60,72,84,
14,28,42,56,70,84,98,

index 392
4,8,12,16,20,24,28,
8,16,24,32,40,48,56,
12,24,36,48,60,72,84,
16,32,48,64,80,96,112,
20,40,60,80,100,120,140,
24,48,72,96,120,144,168,
28,56,84,112,140,168,196,

index 441
6,12,18,24,30,36,42,
12,24,36,48,60,72,84,
18,36,54,72,90,108,126,
24,48,72,96,120,144,168,
30,60,90,120,150,180,210,
36,72,108,144,180,216,252,
42,84,126,168,210,252,294,
-}