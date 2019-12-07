import Data.List (nub)
import Data.Ratio ()

type Capacitance = Rational

-- total capacitance when connecting 2 capacitators in series
series :: Capacitance -> Capacitance -> Capacitance
series a b = a * b / (a + b)

-- total capacitance when connecting 2 capacitators in parallel
parallel :: Capacitance -> Capacitance -> Capacitance
parallel = (+)

-- the base capacitance
base :: Capacitance
base = 60

-- all possible capacitances using (n) capacitors
allPossible :: Int -> [Capacitance]
allPossible 1 = [base]
allPossible n = concatMap f nextLevel
	where
		nextLevel = allPossible $ n - 1
		f c = [series base c, parallel base c]

-- noticed a pattern. this is not necessarily correct.
numPossible :: Integer -> Integer
numPossible n = 2 ^ n - 1

-- using a table for memoization
allPossibleT :: Int -> [Capacitance]
allPossibleT 1 = [base]
allPossibleT n = concatMap f nextLevel
	where
		nextLevel = tbl !! (n - 1)
		f c = [series base c, parallel base c]

tbl :: [[Capacitance]]
tbl = [0] : [base] : [allPossibleT n | n <- [2..]]