module Main where

import Data.List (sort)
import Utils (readInt, printSolution1, deleteIndex, printSolution2)

isIncreasingSequence :: [Int] -> Bool
isIncreasingSequence levels = all id $ map (uncurry (==)) $ zip levels (sort levels)

isDecreasingSequence :: [Int] -> Bool
isDecreasingSequence levels = all id $ map (uncurry (==)) $ zip levels ((reverse . sort) levels)

isGraduallyChanging :: [Int] -> Bool
isGraduallyChanging levels = all isDifferenceGradual $ map (uncurry difference) (zip levels (tail levels))
    where 
        isDifferenceGradual :: Int -> Bool
        isDifferenceGradual diff = (diff <= 3) && (diff >= 1)
        difference :: Int -> Int -> Int
        difference a b = abs (a - b)

isSafe :: [Int] -> Bool
isSafe levels = (isGraduallyChanging levels) && ((isIncreasingSequence levels) || (isDecreasingSequence levels))

alternativeReports :: [Int] -> [[Int]]
alternativeReports levels = [levels] ++ map ((flip deleteIndex) levels) (takeWhile ((>) (length levels))[0..])

isSafeAlternative :: [[Int]] -> Bool
isSafeAlternative alternativeLevels = any id $ map isSafe alternativeLevels

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

solution1 :: String -> Int
solution1 input = sum . map (boolToInt . isSafe) . (map (map readInt . words)) . lines $ input

solution2 :: String -> Int
solution2 input = sum . map (boolToInt . isSafeAlternative) . map alternativeReports . (map (map readInt . words)) . lines $ input

main :: IO()
main = do 
    input <- readFile "inputs/day02.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input