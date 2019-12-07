import Data.Function (on)
import Data.List (sortBy)
import System.Random

-- A position on the game board, from 00 (GO) to 39 (H2).
type Pos = Int

-- The number of a card, from 1 to 16.
type Card = Int

-- Some commonly-used positions bound to a variable for convenience.
go, jail, g2j :: Pos
go   = 00
jail = 10
g2j  = 30

-- These functions return whether a positions is a Community Chest or a Chance
-- position, respectively.
isCC, isCH :: Pos -> Bool
isCC = (`elem` [02, 17, 33])
isCH = (`elem` [07, 22, 36])

gen :: StdGen
gen = mkStdGen 1035

newPosCC, newPosCH :: Card -> Pos -> Pos

-- Given a card number and current position, returns the new position by
-- drawing from the CC cards.
newPosCC card pos = case card of
	1 -> go
	2 -> jail
	_ -> pos

-- Given a card number and current position, returns the new position by
-- drawing from the CH cards.
newPosCH card pos = case card of
	1  -> go
	2  -> jail
	3  -> 11 -- C1
	4  -> 24 -- E3
	5  -> 39 -- H2
	6  -> 05 -- R1
	7  -> nextR pos
	8  -> nextR pos
	9  -> nextU pos
	10 -> pos - 3
	_  -> pos

nextR, nextU :: Pos -> Pos

-- The position of the next railway company.
nextR pos
	| pos <= 04 = 05 -- R1
	| pos <= 14 = 15 -- R2
	| pos <= 24 = 25 -- R3
	| pos <= 34 = 35 -- R4
	| otherwise = 05 -- R1

-- The position of the next utility company.
nextU pos
	| pos <= 11 = 12 -- U1
	| pos <= 27 = 28 -- U2
	| otherwise = 12 -- U1

-- Input: number of sides each die has, random generator
-- Output: (sum of rolls, whether a double was rolled, new random generator)
roll2dice :: Int -> StdGen -> (Int, Bool, StdGen)
roll2dice numSides gen0 = (roll1 + roll2, roll1 == roll2, gen2)
	where
		(roll1, gen1) = randomR (1, numSides) gen0
		(roll2, gen2) = randomR (1, numSides) gen1

-- Input: dice-rolling function, random generator, order of CC cards, order
--     of CH cards
-- Output: infinite list of positions that the player visits
simulate :: (StdGen -> (Int, Bool, StdGen)) -> StdGen -> [Card] -> [Card] -> [Pos]
simulate rollFunc initGen initCC initCH = sim initGen go 0 initCC initCH
	where
		sim :: StdGen -> Pos -> Int -> [Card] -> [Card] -> [Pos]
		sim gen0 pos0 dblCount0 cardsCC@(cc : ccs) cardsCH@(ch : chs) =
			pos2 : sim gen1 pos2 dblCount2 cardsCC1 cardsCH1
			where
				(sumRolls, rolledDouble, gen1) = rollFunc gen0
				pos1 = (pos0 + sumRolls) `mod` 40
				dblCount1 = if rolledDouble
					then dblCount0 + 1
					else 0
				(pos2, dblCount2, cardsCC1, cardsCH1) =
					if dblCount1 == 3 || pos1 == g2j
					then (jail, 0, cardsCC, cardsCH)
					else if isCC pos1
					then (newPosCC cc pos1, dblCount1, ccs ++ [cc], cardsCH)
					else if isCH pos1
					then (newPosCH ch pos1, dblCount1, cardsCC, chs ++ [ch])
					else (pos1, dblCount1, cardsCC, cardsCH)

-- position frequency
posFreq :: [Pos] -> [(Pos, Int)]
posFreq ps = map (\p -> (p, length $ filter (== p) ps)) [00..39]

-- position relative frequency
posRelFreq :: [Pos] -> [(Pos, Double)]
posRelFreq ps = map (\(p, f) -> (p, fromIntegral f / len)) $ posFreq ps
	where len = fromIntegral $ length ps

cards :: [Card]
cards = [5, 16, 9, 7, 11, 2, 8, 3, 15, 13, 12, 10, 4, 1, 6, 14]

listPos :: [Pos]
listPos = simulate (roll2dice 6) gen cards cards

ans :: [(Pos, Double)]
ans = sortBy (compare `on` snd) $ posRelFreq $ take (10^7) listPos