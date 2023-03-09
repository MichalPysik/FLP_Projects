-- Project: FLP Project 1 - Functional Knapsack problem solution in Haskell
-- Author: Michal Pysik (login: xpysik00)
-- Year: 2023
-- Module: BruteForce

module BruteForce (bruteForce) where

import Data.List (sortBy)
import Types (Knapsack(..), Item(..))


-- Brute force algorithm for solving the knapsack problem
bruteForce :: Knapsack -> Either Bool [Int]
bruteForce knapsack
    | sumItemCost (items knapsack) < (minCost knapsack) = Left False
    | otherwise = Right $ head $ sortBy (\a b -> compare (permutItemCost b (items knapsack)) (permutItemCost a (items knapsack))) $ filterMaxWeight knapsack
            

filterMaxWeight :: Knapsack -> [[Int]]
filterMaxWeight knapsack = filter (\p -> (permutItemWeight p (items knapsack)) <= (maxWeight knapsack)) (allBinPermuts $ length (items knapsack))





sumItemCost :: [Item] -> Int
sumItemCost = foldr (\item acc -> acc + (cost item)) 0

sumItemWeight :: [Item] -> Int
sumItemWeight = foldr (\item acc -> acc + (weight item)) 0

allBinPermuts :: Int -> [[Int]]
allBinPermuts 0 = [[]]
allBinPermuts n = [0:xs | xs <- allBinPermuts (n-1)] ++ [1:xs | xs <- allBinPermuts (n-1)]

permutItemCost :: [Int] -> [Item] -> Int
permutItemCost permut items = foldr (\zipped acc -> acc + if (fst zipped) == 1 then (cost $ snd zipped) else 0) 0 (zip permut items)

permutItemWeight :: [Int] -> [Item] -> Int
permutItemWeight permut items = foldr (\zipped acc -> acc + if (fst zipped) == 1 then (weight $ snd zipped) else 0) 0 (zip permut items)

