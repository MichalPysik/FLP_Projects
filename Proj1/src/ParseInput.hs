-- Project: FLP Project 1 - Functional Knapsack problem solition in Haskell
-- Author: Michal Pysik (login: xpysik00)
-- Year: 2023
-- Module: ParseInput

module ParseInput where

import qualified Text.Parsec as Parsec (Parsec, parse, string, char, digit, many, many1, ParseError)
import Types (Knapsack(..), Item(..))


-- Parses a string into Knapsack data type
parseInput :: String -> Knapsack
parseInput input = parseInput' $ Parsec.parse knapsackParser "Invalid input" input
    where
        -- Helper function for parseInput that handles errors
        parseInput' :: Either Parsec.ParseError Knapsack -> Knapsack
        parseInput' (Left err) = error $ show err
        parseInput' (Right knapsack) = knapsack

-- Parser for Knapsack data type
knapsackParser :: Parsec.Parsec String () Knapsack
knapsackParser = do
    _ <- Parsec.string "Knapsack{"
    _ <- Parsec.string "maxWeight:"
    maxWeight <- Parsec.many1 Parsec.digit
    _ <- Parsec.string "minCost:"
    minCost <- Parsec.many1 Parsec.digit
    _ <- Parsec.string "items:["
    items <- Parsec.many itemParser
    _ <- Parsec.string "]}"
    return $ Knapsack (read maxWeight) (read minCost) items

-- Parser for Item data type
itemParser :: Parsec.Parsec String () Item
itemParser = do 
    _ <- Parsec.string "Item{"
    _ <- Parsec.string "weight:"
    weight <- Parsec.many1 Parsec.digit
    _ <- Parsec.string "cost:"
    cost <- Parsec.many1 Parsec.digit
    _ <- Parsec.char '}'
    return $ Item (read weight) (read cost)
    
