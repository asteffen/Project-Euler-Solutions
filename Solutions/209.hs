xor :: Bool -> Bool -> Bool
xor = (/=)

-- k-input binary truth table
allCombs :: Int -> [[Bool]]
allCombs 0 = [[]]
allCombs k = map (False:) rest ++ map (True:) rest
	where rest = allCombs $ k - 1