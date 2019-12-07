{-
Pattern:

1    3    5    7    9    13   17   21   25   31   37   43   49   ...
  +2   +2   +2   +2   +4   +4   +4   +4   +6   +6   +6   +6   ...
-}

-- [(+2),(+2),(+2),(+2),(+4),(+4),(+4),(+4),(+6),(+6),(+6),(+6),...]
func_list = concatMap (replicate 4 . (+)) [2,4..]

-- [1,3,5,7,9,13,17,21,25,31,37,43,49,...]
spiral_diags = helper 1 func_list
	where helper n (f:fs) = n : helper (f n) fs

-- the last number in a 5 by 5 spiral is 5^2, so
-- the last number in a 1001 by 1001 spiral is 1001^2.
main = print $ sum $ takeWhile (<= 1001^2) spiral_diags