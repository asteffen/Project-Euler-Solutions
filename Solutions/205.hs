--{-# OPTIONS_GHC +RTS -K100M -RTS #-}

import System.Random

myGen :: StdGen
myGen = mkStdGen 77

-- rolls (numDice) (numSides)-sided dice.
-- returns the sum of the rolls and a new generator.
rollDice :: Int -> Int -> StdGen -> (Int, StdGen)
rollDice numSides numDice gen = (sum $ map fst rolls, snd (last rolls))
	where
		rolls = rollDice' numDice gen
		rollDice' 0 gen = []
		rollDice' numDice gen = (rand, gen) : rollDice' (numDice - 1) newGen
			where (rand, newGen) = randomR (1, numSides) gen

-- rolls 9 4-sided dice.
peterRoll :: StdGen -> (Int, StdGen)
peterRoll = rollDice 4 9

-- roll 6 6-sided dice.
colinRoll :: StdGen -> (Int, StdGen)
colinRoll = rollDice 6 6

-- given a generator and the number of simulations to run,
-- returns the number of times Peter won.
-- (peterWins is supposed to be initialized to 0.)
simulate :: StdGen -> Int -> Int -> Int
simulate gen1 0 peterWins = peterWins
simulate gen1 numSims peterWins = (simulate gen3 $! numSims - 1) newPeterWins
	where
		(peterScore, gen2) = peterRoll gen1
		(colinScore, gen3) = colinRoll gen2
		newPeterWins = if peterScore > colinScore
			then peterWins + 1
			else peterWins

-- given a number of simulations, returns the approximate probability that Peter wins.
probPeter :: Int -> Double
probPeter numSims = fromIntegral wins / fromIntegral numSims
	where wins = simulate myGen numSims 0

main :: IO ()
main = print $ probPeter 30000