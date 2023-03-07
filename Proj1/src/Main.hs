-- Project: FLP Project 1 - Functional Knapsack problem solution in Haskell
-- Author: Michal Pysik (login: xpysik00)
-- Year: 2023
-- Module: Main

module Main where

import System.Environment (getArgs)
import Data.Char (isSpace)
import Types (Knapsack(..), Item(..))
import ParseInput (parseInput)
import BruteForce (bruteForce)


-- Main function
main :: IO ()
main = do
    args <- getArgs
    let (actions, filepath) = parseArgs args
    input <- (if filepath /= "" then readFile filepath else getContents)
    let knapsack = parseInput $ filter (not . isSpace) input
    if "instance" `elem` actions
        then print knapsack
        else return ()
    if "brute" `elem` actions
        then print $ bruteForce knapsack
        else return ()
    if "optimized" `elem` actions
        then putStrLn "Optimized selected"
        else return ()


-- Parses command line arguments (specialized interface for parseArgs' function)
-- Returns a tuple of a list of actions and a filepath
parseArgs :: [String] -> ([String], String)
parseArgs args = parseArgs' args ([], "")

-- Transforms a tuple of a list of actions and a filepath based on the given list of arguments
parseArgs' :: [String] -> ([String], String) -> ([String], String)
parseArgs' [] ([], _) = error "No options selected"
parseArgs' [] (actions, filepath) = (actions, filepath)
parseArgs' (x:xs) (actions, filepath)
    | x == "-i" = parseArgs' xs ((actions ++ ["instance"]), "")
    | x == "-b" = parseArgs' xs ((actions ++ ["brute"]), "")
    | x == "-o" = parseArgs' xs ((actions ++ ["optimized"]), "")
    | head x == '-' = error ("Unknown option \'" ++ x ++ "\'")
    | filepath == "" = parseArgs' xs (actions, x)
    | otherwise = error "Too many arguments"



