import Text.ParserCombinators.Parsec
import System.IO.Unsafe (unsafePerformIO)
import Data.List (sort, group)

data Card = Card {value :: Int, suit :: Int}
	deriving (Show, Eq, Ord)

type Hand = [Card]

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

parseValue :: Parser Int
parseValue = do
	value <- oneOf "23456789TJQKA"
	return $ case value of
		'2' -> 2
		'3' -> 3
		'4' -> 4
		'5' -> 5
		'6' -> 6
		'7' -> 7
		'8' -> 8
		'9' -> 9
		'T' -> 10
		'J' -> 11
		'Q' -> 12
		'K' -> 13
		'A' -> 14

parseSuit :: Parser Int
parseSuit = do
	suit <- oneOf "CDHS"
	return $ case suit of
		'C' -> 0
		'D' -> 1
		'H' -> 2
		'S' -> 3

parseCard :: Parser Card
parseCard = do
	value <- parseValue
	suit <- parseSuit
	optional $ char ' '
	return $ Card value suit

parseHand :: Parser Hand
parseHand = do
	c1 <- parseCard
	c2 <- parseCard
	c3 <- parseCard
	c4 <- parseCard
	c5 <- parseCard
	return [c1, c2, c3, c4, c5]

parseHands :: Parser (Hand, Hand)
parseHands = do
	h1 <- parseHand
	h2 <- parseHand
	return (h1, h2)

readHand :: String -> Hand
readHand input = case parse parseHand "hands" input of
	Left err  -> error $ show err
	Right val -> val

readHands :: String -> (Hand, Hand)
readHands input = case parse parseHands "hands" input of
	Left err  -> error $ show err
	Right val -> val

--------------------------------------------------------------------------------
-- Testing Hands
--------------------------------------------------------------------------------

{-
Some statistics I gathered using the numberOf function.

Rank          | Occurences
onePair       | 950
twoPairs      | 103
threeOfAKind  | 38
straight      | 12
flush         | 2
fullHouse     | 2
fourOfAKind   | 0
straightFlush | 0
royalFlush    | 0

The full houses are not in the same line.
The flushes are not in the same line.
The straights are all in different lines.
-}

values, suits :: Hand -> [Int]
values = map value
suits = map suit

multiples :: Hand -> [Int]
multiples = sort . map length . group . sort . values

-- number of hands in the file that satisfy (f).
numberOf :: (Hand -> Bool) -> Int
numberOf f = length $ filter id $ concatMap (\(h1, h2) -> [f h1, f h2]) hands

onePair, twoPairs, threeOfAKind, straight, flush, fullHouse, fourOfAKind,
	straightFlush, royalFlush :: Hand -> Bool

onePair = (2 `elem`) . multiples

twoPairs = (== [1, 2, 2]) . multiples

threeOfAKind = (3 `elem`) . multiples

straight hand = xx == [x .. x + 4]
	where xx@(x:_) = sort $ values hand

flush hand = all (== x) xs
	where (x:xs) = suits hand

fullHouse = (== [2, 3]) . multiples

fourOfAKind = (4 `elem`) . multiples

straightFlush hand = straight hand && flush hand

royalFlush hand = straightFlush hand && highCard hand == [14]

--------------------------------------------------------------------------------
-- Scoring Hands
--------------------------------------------------------------------------------

{-
This scoring system takes advantage of the fact that lists of Ints can be
compared lexographically.

Each score function maps a Hand to an [Int].
The totalScore function puts all of them in order of precedence, so that
the scores can be compared by a simple (>).

Note: scoreFourOfAKind, scoreStraightFlush, and scoreRoyalFlush are not implemented
because they never appear; however, they should be trivial to implement.
-}

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

-- organizes the cards into (occurences, value) pairs,
-- sorted by the occurences in descending order.
multiples2 :: Hand -> [(Int, Int)]
multiples2 = reverse . sort . map (\xs -> (length xs, head xs)) . group . sort . values

scoreFullHouse, scoreFlush, scoreStraight, scoreThreeOfAKind, scoreTwoPairs,
	scoreOnePair, highCard :: Hand -> [Int]

highCard hand = [maximum $ values hand]

scoreOnePair hand = case multiples2 hand of
	(2, pair) : others -> pair : map snd others
	-- [(2, pair), (1, other1), (1, other2), (1, other3)] -> [pair, other1, other2, other3]
	_ -> []

scoreTwoPairs hand = case multiples2 hand of
	[(2, pair1), (2, pair2), (1, other)] -> [pair1, pair2, other]
	_ -> []

scoreThreeOfAKind hand = case multiples2 hand of
	[(3, triple), (1, other1), (1, other2)] -> [triple, other1, other2]
	_ -> []

scoreStraight hand = if straight hand
	then highCard hand
	else []

scoreFlush hand = if flush hand
	then highCard hand
	else []

scoreFullHouse hand = case multiples2 hand of
	[(3, triple), (2, pair)] -> [triple, pair]
	_ -> []

totalScore :: Hand -> [[Int]]
totalScore hand = map ($ hand) [scoreFullHouse, scoreFlush, scoreStraight,
	scoreThreeOfAKind, scoreTwoPairs, scoreOnePair, highCard]

player1wins :: (Hand, Hand) -> Bool
player1wins (p1, p2) = totalScore p1 > totalScore p2

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

hands :: [(Hand, Hand)]
hands = map readHands $ lines $ unsafePerformIO $ readFile "54.txt"

main :: IO ()
main = print $ length $ filter player1wins hands