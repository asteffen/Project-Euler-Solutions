import Data.Ratio

-- Returns a list of all distinct lists of (Bool)s of length (n).
allLists :: Int -> [[Bool]]
allLists 0 = [[]]
allLists n = map (False :) rest ++ map (True :) rest
	where rest = allLists $ n - 1

{-
Given a list (Bool)s where True represents a win and False represents a loss,
returns the probability of the outcome.

The first element in the given list is the outcome of the first turn, the
second is the outcome of the second turn, and so on.

Note: The probability of winning the n-th turn is (1 / (n + 1)).
-}
probOutcome :: [Bool] -> Rational
probOutcome = product . zipWith (\frac bool -> if bool then frac else 1 - frac) (map (1 %) [2..])

-- The probability that the player will win when the game is played for (n) turns.
probWin :: Int -> Rational
probWin n = sum $ map probOutcome $ filter ((> n `div` 2) . length . filter id) $ allLists n

-- The maximum prize fund the banker should allocate before they would expect to incur a loss.
prizeFund :: Int -> Integer
prizeFund = floor . fromRational . (1 /) . probWin

main :: IO ()
main = print $ prizeFund 15