{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.List ( subsequences )
import Control.Arrow ( Arrow((&&&)) )
import ChallengeUtils (readInt, boolToInt, printSolution1, printSolution2)

data Operator = ADDITION | MULTIPLICATION | CONCATENATION deriving (Show)

concatenate :: Int -> Int -> Int
concatenate a b = readInt $ (show a) ++ (show b) 

operator :: Operator -> (Int -> Int -> Int)
operator ADDITION = (+)
operator MULTIPLICATION = (*)
operator CONCATENATION = concatenate

operatorPermutations :: Int -> [[Operator]]
operatorPermutations n = map (
    \additionPosition 
    -> map (
        \index 
        -> if index `elem` additionPosition 
            then ADDITION 
            else MULTIPLICATION) 
            [0..n-1]) 
            $ subsequences [0..n-1]

operatorPermutations3 :: Int -> [[Operator]]
operatorPermutations3 n = concat 
    $ map (
        \(nonMultiplicationPosition, additionPositionPermutations) 
        -> map (
            \additionPositions 
            -> map (
                \index 
                -> if index `elem` nonMultiplicationPosition 
                    then (if index `elem` additionPositions 
                            then ADDITION 
                            else CONCATENATION) 
                    else MULTIPLICATION) 
                    [0..n-1]
                ) additionPositionPermutations
            ) $ map (id &&& subsequences) $ subsequences [0..n-1]
    

solve :: (Int -> [[Operator]]) -> Int -> [Int] -> Bool
solve operatorPermutationF result (firstNumber:restOfNumbers) = do 
    let operatorPerms = operatorPermutationF (length restOfNumbers)
    any ((==) result . foldl (\aggr (n, op) -> (operator op) aggr n) firstNumber  . zip restOfNumbers) operatorPerms    

solution1 :: String -> Int
solution1 input = do
    let equations = map ((\(a:b:[]) -> ((readInt . T.unpack) a, (map (readInt . T.unpack) . T.words) b)) . T.splitOn (T.pack ":") . T.pack) $ lines input
    sum $ map snd $ filter fst $ zip (map (uncurry (solve operatorPermutations)) equations) (map fst equations)

solution2 :: String -> Int
solution2 input = do
    let equations = map ((\(a:b:[]) -> ((readInt . T.unpack) a, (map (readInt . T.unpack) . T.words) b)) . T.splitOn (T.pack ":") . T.pack) $ lines input
    sum $ map snd $ filter fst $ zip (map (uncurry (solve operatorPermutations3)) equations) (map fst equations)
    
main :: IO()
main = do
    input <- readFile "inputs/day07.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input
