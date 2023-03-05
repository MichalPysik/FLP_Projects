-- Project: FLP Project 1 - Functional Knapsack problem solution in Haskell
-- Author: Michal Pysik (login: xpysik00)
-- Year: 2023
-- Module: Types

module Types (Knapsack(..), Item(..), prepTabs) where


data Item = Item { weight :: Int
                 , cost :: Int
                 }

instance Show Item where
    show (Item w c) = "Item {\nweight: " ++ show w ++ "\ncost: " ++ show c ++ "\n}"


data Knapsack = Knapsack { maxWeight :: Int
                         , minCost :: Int
                         , items :: [Item]
                         }

instance Show Knapsack where
    show (Knapsack mw mc its) = "Knapsack {\nmaxWeight: " ++ show mw ++ "\nminCost: " ++ show mc ++ "\nitems: [" ++ formatItems its ++ "]\n}"


formatItems :: [Item] -> String
formatItems [] = ""
formatItems its = "\n" ++ (prepTabs $ foldl (\acc it -> acc ++ show it ++ "\n") "" its)

prepTabs :: String -> String
prepTabs = unlines . map (\l -> "\t" ++ l) . lines
