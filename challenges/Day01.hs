module Main where

import Data.List (transpose, sort)
import ChallengeUtils ( printSolution1, printSolution2, makeCounter, (!), readInt)


getDistance :: [Int] -> Int
getDistance (a:b:[]) = abs (a - b)
getDistance _ = 0

solution1 :: String -> Int
solution1 input = sum . map getDistance . transpose . map sort . map (map readInt) . transpose . (map words . lines) $ input

solution2 :: String -> Int
solution2 input = do 
    let first:second:[] = map (map readInt) . transpose . (map words . lines) $ input
    sum . map (uncurry (*)) $ zip (map ((!) (makeCounter second)) first) first

main :: IO()
main = do
    input <- readFile "inputs/day01.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input