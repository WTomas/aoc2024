{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Set as Set
import ChallengeUtils (readInt, makeCounter, size, Counter (Counter, _map), incrementCounterBy, printSolution1, printSolution2)
import qualified Data.Map as M

data Stone = Stone Int deriving (Show, Eq, Ord)

blinkAtStone :: Stone -> [Stone]
blinkAtStone (Stone 0) = (Stone 1):[]
blinkAtStone (Stone d) = case (length . show) d `mod` 2 == 0 of
    True -> do
        let indexedDigits = zip [0..] (show d)
        let halfPoint = length indexedDigits `div` 2
        let firstN = readInt $ map snd $ takeWhile ((<halfPoint) . fst) indexedDigits
        let secondN = readInt $ map snd $ dropWhile ((<halfPoint) . fst) indexedDigits
        [Stone firstN, Stone secondN]        
    False -> (Stone (d * 2024)):[]

blinkAtStones :: Counter Stone -> Counter Stone
blinkAtStones (Counter { _map }) = do
    M.foldrWithKey f (makeCounter []) _map
    where
        f :: Stone -> Int -> Counter Stone -> Counter Stone
        f stone count counter = do
            let newStones = blinkAtStone stone
            foldl (\counter' newStone -> incrementCounterBy counter' newStone count) counter newStones

blinkAtStonesNTimes :: Counter Stone -> Int -> Counter Stone
blinkAtStonesNTimes stones n = iterate blinkAtStones stones !! n

solution1 :: String -> Int
solution1 input = do
    let stones = map (Stone . readInt) $ words input 
    M.foldr (+) 0 $ _map $ blinkAtStonesNTimes (makeCounter stones) 25

solution2 :: String -> Int
solution2 input = do
    let stones = map (Stone . readInt) $ words input 
    M.foldr (+) 0 $ _map $ blinkAtStonesNTimes (makeCounter stones) 75

main :: IO()
main = do
    input <- readFile "inputs/day11.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input
    