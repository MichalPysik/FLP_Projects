-- Project: FLP Project 1 - Functional Knapsack problem solution in Haskell
-- Author: Michal Pysik (login: xpysik00)
-- Year: 2023
-- Module: BruteForce

module BruteForce (bruteForce) where

import Types (Knapsack(..), Item(..))


-- Brute force algorithm for solving the knapsack problem
bruteForce :: Knapsack -> [Int]
bruteForce (Knapsack mw mc its) = bruteForce' its 0 0 [] []
    where
        -- Helper function for bruteForce that handles the recursion
        bruteForce' :: [Item] -> Int -> Int -> [Int] -> [Int] -> [Int]
        bruteForce' [] _ _ _ best = best
        bruteForce' (it:its) w c is best
            | w + weight it <= mw && c + cost it >= mc = bruteForce' its (w + weight it) (c + cost it) (is ++ [1]) (is ++ [1])
            | otherwise = bruteForce' its w c (is ++ [0]) best