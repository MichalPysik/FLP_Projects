-- Project: FLP Project 1 - Functional Knapsack problem solution in Haskell
-- Author: Michal Pysik (login: xpysik00)
-- Year: 2023
-- Module: Types

module Types (Knapsack(..), Item(..), prepSpaces) where


-- Item data type along with its Show instance
data Item = Item { weight :: Int
                 , cost :: Int
                 }

instance Show Item where
    show (Item w c) = "Item {\nweight: " ++ show w ++ "\ncost: " ++ show c ++ "\n}"


-- Knapsack data type along with its Show instance
data Knapsack = Knapsack { maxWeight :: Int
                         , minCost :: Int
                         , items :: [Item]
                         }

instance Show Knapsack where
    show (Knapsack mw mc its) = "Knapsack {\nmaxWeight: " ++ show mw ++ "\nminCost: " ++ show mc ++ "\nitems: [" ++ formatItems its ++ "]\n}"
        where
            -- Formats a list of items into a specifically formatted string
            formatItems :: [Item] -> String
            formatItems [] = ""
            formatItems its' = "\n" ++ (prepSpaces $ foldr (\it' acc -> acc ++ show it' ++ "\n") "" its')


-- Prepends four spaces to each line of a string
prepSpaces :: String -> String
prepSpaces = unlines . map (\l -> "    " ++ l) . lines
