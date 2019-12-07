import Data.Ratio

data RealNum
	= RealInt Integer
	| RealNum :+ RealNum
	| RealNum :- RealNum
	| RealNum :* RealNum
	| RealNum :/ RealNum
	| RealNum :^ RealNum
	deriving (Show)

toDouble :: RealNum -> Double
toDouble (RealInt a) = fromIntegral a
toDouble (a :+ b) = toDouble a + toDouble b
toDouble (a :- b) = toDouble a - toDouble b
toDouble (a :* b) = toDouble a * toDouble b
toDouble (a :/ b) = toDouble a / toDouble b
toDouble (a :^ b) = toDouble a ** toDouble b