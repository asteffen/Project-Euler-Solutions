{-
the inverse function does not work on certain matrices. ex:
	[[11,16,0],[23,12,21],[1,23,25]] (produces wrong matrix)
	[[14,19,13],[17,2,15],[21,17,25]] (says it is noninvertible when it is actually
		invertible).
In both cases, I was working mod 26.
-}

module Matrix where

import Data.List (transpose, intersperse, maximumBy)
import Data.Function (on)

data Matrix a = Matrix [[a]]
	deriving (Eq)

instance (Show a) => Show (Matrix a) where
	show (Matrix a) = concat . intersperse "\n" $ map showRow a
		where showRow = concat . intersperse ", " . map show

instance Functor Matrix where
	fmap f (Matrix a) = Matrix $ map (map f) a

instance (Num a) => Num (Matrix a) where
	(+) = addM
	(*) = mulM
	(-) = subM
	negate = fmap negate
	fromInteger n = Matrix [[fromIntegral n]]
	
	-- placeholders
	abs = id
	signum = id

fromMatrix :: Matrix a -> [[a]]
fromMatrix (Matrix a) = a

width, height :: Matrix a -> Int
width  (Matrix a) = length $ head a
height (Matrix a) = length a

-- matrix addition, subtraction, and multiplication.
addM, subM, mulM :: (Num a) => Matrix a -> Matrix a -> Matrix a
addM (Matrix a) (Matrix b) = Matrix $ zipWith (zipWith (+)) a b
subM (Matrix a) (Matrix b) = Matrix $ zipWith (zipWith (-)) a b

mulM matA@(Matrix a) matB@(Matrix b)
	| width matA /= height matB = error "mulM: dim mismatch"
	| otherwise = Matrix [map (sum . zipWith (*) xs) b' | xs <- a]
	where b' = transpose b

-- swap the elements of a list at the 2 specified indices.
swapElems :: Int -> Int -> [a] -> [a]
swapElems index1 index2 xs = map mapF zippedList
	where
		zippedList = zip [0..] xs
		mapF (index, elem)
			| index == index1 = xs !! index2
			| index == index2 = xs !! index1
			| otherwise       = elem

-- using row operations, transforms column (n) of the matrix so that
-- the (n,n)th element is 1, and all other elements in the column are 0.
transformCol :: (Ord a, Fractional a) => Int -> Matrix a -> Matrix a
transformCol col (Matrix a) = Matrix $ map mapF zippedM
	where
		maxRow = fst . maximumBy (compare `on` snd) . zip [0..]
		
		-- matrix A, where row (col) and (the maximum row below it) are swapped.
		-- the reason the rows are swapped is to avoid dividing by 0 and also
		-- for numerical stability.
		swappedM = swapElems col (col + maxRow (drop col a)) a
		
		-- current row, normalized so that the first nonzero element is 1.
		normalized = map (/ currentRow !! col) currentRow
			where currentRow = swappedM !! col
		
		-- swappedM, with the rows numbered.
		zippedM = zip [0..] swappedM
		
		-- function to map over each row in the zipped matrix
		mapF (rowNum, elems) = if rowNum == col
			then normalized
			else zipWith (\norm elem -> elem - multiplier * norm) normalized elems
			where multiplier = elems !! col

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 rep (_:xs) = rep : xs
replaceAt index rep (x:xs) = x : replaceAt (index - 1) rep xs

-- identity matrix of a specified size.
identityM :: (Num a) => Int -> Matrix a
identityM size = Matrix $ map mapF [0 .. size - 1]
	where mapF index = replaceAt index 1 $ replicate size 0

-- augment two matrices together.
augment :: Matrix a -> Matrix a -> Matrix a
augment matA@(Matrix a) matB@(Matrix b)
	| height matA /= height matB = error "augment: dim mismatch"
	| otherwise = Matrix $ zipWith (++) a b

-- inverse of a matrix found by using an augmented matrix and row operations.
inverse :: (Ord a, Fractional a) => Matrix a -> Matrix a
inverse matA = Matrix rightHalf
	where
		size = width matA
		aug = augment matA $ identityM size
		(Matrix result) = foldl (\acc col -> transformCol col acc)
			aug [0 .. size - 1]
		rightHalf = map (drop size) result