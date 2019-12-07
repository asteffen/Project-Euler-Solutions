import Data.List (delete, genericReplicate)

powersOf2 :: [Integer]
powersOf2 = iterate (* 2) 1

ways 0 _ = [[]]
ways _ [] = []
ways n (x:xs)
	| n < 0 = [] -- not necessary, but speeds it up
	| otherwise = concatMap f [0..2]
	where f r = map (g ++) rest
		where
			rest = ways (n - x * r) xs
			g = genericReplicate r x

ans n = ways n $ reverse $ takeWhile (<= n) powersOf2

toBinary 0 = []
toBinary n = toBinary d ++ [m]
	where (d, m) = n `divMod` 2