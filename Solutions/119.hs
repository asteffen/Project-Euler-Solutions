import Data.Char (digitToInt)
import Data.List (sort)

digitalSum :: Integer -> Int
digitalSum = sum . map digitToInt . show

-- tries to find a power of n with a digital sum of n.
-- 100 is an arbitrary bound.
findQual :: Integer -> [Integer]
findQual n = filter ((== n') . digitalSum) $ take 100 $ iterate (* n) (n * n)
	where n' = fromIntegral n :: Int

-- sequence of a_n.
aSeq :: [Integer]
aSeq = sort $ concatMap findQual [2..100]

main = print $ aSeq !! 29