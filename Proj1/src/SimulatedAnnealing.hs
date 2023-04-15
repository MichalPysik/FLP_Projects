-- Project: FLP Project 1 - Functional Knapsack problem solution in Haskell
-- Author: Michal Pysik (login: xpysik00)
-- Year: 2023
-- Module: SimulatedAnnealing

module SimulatedAnnealing (simulatedAnnealing) where

import System.Random(StdGen, randomR)
import Types (Knapsack(..))
import BruteForce(permutItemCost, permutItemWeight)


-- Simulated annealing algorithm for solving the knapsack problem (solution may not be optimal)
-- Returns either an Int list corresponding to the selected items or False indicating that no solution had been found during the run
simulatedAnnealing :: Knapsack -> StdGen -> Double -> Double -> Int -> Either Bool [Int]
simulatedAnnealing knapsack gen alpha initTemp maxIters =
    let initState = take (length (items knapsack)) [0,0..]
        state = fst $ simulatedAnnealing' knapsack initState gen alpha initTemp 0 maxIters
    in if permutItemCost state (items knapsack) < (minCost knapsack) then Left False else Right state

-- The main functional part of the simulated annealing algorithm
-- Returns the best state found during the run (and the final generator)
simulatedAnnealing' :: Knapsack -> [Int] -> StdGen -> Double -> Double -> Int -> Int -> ([Int], StdGen)
simulatedAnnealing' knapsack state gen alpha initTemp iter maxIters
    | permutItemCost state (items knapsack) >= (minCost knapsack) || iter == maxIters = (state, gen)
    | permutCmp knapsack state newState = simulatedAnnealing' knapsack newState gen' alpha initTemp (iter + 1) maxIters
    | permutItemWeight newState (items knapsack) <= (maxWeight knapsack) && accept = simulatedAnnealing' knapsack newState gen'' alpha initTemp (iter + 1) maxIters
    | otherwise = simulatedAnnealing' knapsack state gen'' alpha initTemp (iter + 1) maxIters
    where
        (newState, gen') = flipNeighbor state gen
        (accept, gen'') = acceptance (permutItemCost state (items knapsack)) (permutItemCost newState (items knapsack)) (tempFunc initTemp alpha iter) gen'

-- Calculates the temperature for the given iteration
tempFunc :: Double -> Double -> Int -> Double
tempFunc initTemp alpha iter = initTemp * (alpha ** (fromIntegral iter))

-- Flips a random item in the given Int list (either adds or removes it from the solution generating a new state)
flipNeighbor :: [Int] -> StdGen -> ([Int], StdGen)
flipNeighbor state gen =
    let (index, gen') = randomR (0, (length state) - 1) gen
        (val, gen'') = randomR (0, 1) gen'
        (before, after) = splitAt index state
    in (before ++ [val] ++ (tail after), gen'')

-- Returns True if state 2 totals to a better solution than state 1 (no need for probability calculation)
permutCmp :: Knapsack -> [Int] -> [Int] -> Bool
permutCmp knapsack state newState =
    let cost = permutItemCost state (items knapsack)
        costNew = permutItemCost newState (items knapsack)
        weightNew = permutItemWeight newState (items knapsack)
    in costNew > cost && weightNew <= (maxWeight knapsack)

-- Returns True if the new worse state should be accepted (probability calculation based on the current temperature)
acceptance :: Int -> Int -> Double -> StdGen -> (Bool, StdGen)
acceptance cost costNew temp gen = (exp ((fromIntegral (cost - costNew)) / temp) > randVal, gen')
    where
        (randVal, gen') = randomR (0.0, 1.0) gen




    










-- Generates an initial state represented by the selected items
--initState :: [Item] -> StdGen -> ([Int], StdGen)
--initState [] gen = ([], gen)
--initState (_:its) gen =
--    let (val, gen') = randomR (0, 1) gen
--        (rest, gen'') = initState its gen'
--    in (val:rest, gen'')
