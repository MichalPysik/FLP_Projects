-- Project: FLP Project 1 - Functional Knapsack problem solution in Haskell
-- Author: Michal Pysik (login: xpysik00)
-- Year: 2023
-- Module: Main

module Main where

import System.Environment (getArgs)
import Types (Knapsack(..), Item(..))
--import ParseInput


main :: IO ()
main = do
    args <- getArgs
    let (actions, filename) = parseArgs args ([], "")
    if "instance" `elem` actions
        then putStrLn "Instance selected"
        else return ()
    if "brute" `elem` actions
        then putStrLn "Brute selected"
        else return ()
    if "optimized" `elem` actions
        then putStrLn "Optimized selected"
        else return ()


parseArgs :: [String] -> ([String], String) -> ([String], String)
parseArgs [] ([], _) = error "No options selected"
parseArgs [] (actions, filename) = (actions, filename)
parseArgs (x:xs) (actions, filename)
    | x == "-i" = parseArgs xs ((actions ++ ["instance"]), "")
    | x == "-b" = parseArgs xs ((actions ++ ["brute"]), "")
    | x == "-o" = parseArgs xs ((actions ++ ["optimized"]), "")
    | filename == "" = parseArgs xs (actions, x)
    | otherwise = error "Too many arguments"



