import Time

squareDigits :: [(Int, Int)]
squareDigits =
	[(0,1)
	,(0,4)
	,(0,9)
	,(1,6)
	,(2,5)
	,(3,6)
	,(4,9)
	,(6,4)
	,(8,1)
	]

type Die = [Int]

-- returns if a die contains a certain digit. handles 6 and 9 as special cases.
containsDigit :: Die -> Int -> Bool
die `containsDigit` n
	| n == 6 || n == 9 = contains 6 || contains 9
	| otherwise        = contains n
	where contains = (`elem` die)

-- returns if the two dice can display all squareDigits
canDisplayAll :: Die -> Die -> Bool
canDisplayAll die1 die2 = all canDisplay squareDigits
	where
		canDisplay (t, u) = canDisplayInOrder t u || canDisplayInOrder u t
		canDisplayInOrder a b = (die1 `containsDigit` a) && (die2 `containsDigit` b)

-- All distinct dice. Only counts each order once, because a <= b <= c <= d <= e <= f.
allDice :: [Die]
allDice = [[a,b,c,d,e,f] | a <- [0..9], b <- [a..9], c <- [b..9], d <- [c..9],
	e <- [d..9], f <- [e..9]]

-- The dice cannot be equal, because if they are, then there is a maximum of 
-- 6 distinct digits. But 8 distinct digits are required: 0,1,2,3,4,5,6,8.
pairsPossible :: [(Die, Die)]
pairsPossible = [(die1, die2) | die1 <- allDice, die2 <- allDice,
	die1 < die2, canDisplayAll die1 die2]

main = timePrint $ length $ pairsPossible