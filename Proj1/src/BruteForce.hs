-- Project: FLP Project 1 - Functional Knapsack problem solution in Haskell
-- Author: Michal Pysik (login: xpysik00)
-- Year: 2023
-- Module: BruteForce

module BruteForce (bruteForce, permutItemCost, permutItemWeight) where

import Data.List (sortBy)
import Types (Knapsack(..), Item(..))


-- Brute force algorithm for solving the knapsack problem (optimal solution)
-- Returns either an Int list corresponding to the selected items or False indicating that the problem is unsolvable
bruteForce :: Knapsack -> Either Bool [Int]
bruteForce knapsack
    | null $ bruteForce' knapsack = Left False
    | otherwise = Right $ head $ bruteForce' knapsack

-- Creates a list of all possible item permutations, filters them by the maximum weight and minimum cost, and sorts them by weight in descending order
bruteForce' :: Knapsack -> [[Int]]
bruteForce' knapsack = sortBy (\a b -> compare (permutItemCost b (items knapsack)) (permutItemCost a (items knapsack))) $ filterPermuts knapsack

-- Filters permutations that either exceed the maximum weight or do not meet the minimum cost
filterPermuts :: Knapsack -> [[Int]]
filterPermuts knapsack = filter (permutFilter knapsack) (allBinPermuts $ length (items knapsack))

-- Checks if a permutation of items meets the maximum weight and minimum cost requirements
permutFilter :: Knapsack -> [Int] -> Bool
permutFilter knapsack p = (permutItemWeight p (items knapsack)) <= (maxWeight knapsack) && (permutItemCost p (items knapsack)) >= (minCost knapsack)

-- Generates all binary permutations of a given length
allBinPermuts :: Int -> [[Int]]
allBinPermuts 0 = [[]]
allBinPermuts n = [0:xs | xs <- allBinPermuts (n-1)] ++ [1:xs | xs <- allBinPermuts (n-1)]

-- Calculates the total cost of a permutation of items
permutItemCost :: [Int] -> [Item] -> Int
permutItemCost permut its = foldr (\zipped acc -> acc + if (fst zipped) == 1 then (cost $ snd zipped) else 0) 0 (zip permut its)

-- Calculates the total weight of a permutation of items
permutItemWeight :: [Int] -> [Item] -> Int
permutItemWeight permut its = foldr (\zipped acc -> acc + if (fst zipped) == 1 then (weight $ snd zipped) else 0) 0 (zip permut its)



