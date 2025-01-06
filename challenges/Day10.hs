module Main where

import ChallengeUtils (Position, readInt, flatMatrix, printSolution1, printSolution2)
import qualified Data.Set as Set

data Trail = Trail Int deriving (Show)
type TrailPos = (Position, Trail)

isTrailhead :: Trail -> Bool
isTrailhead (Trail elevation) = elevation == 0

isTrailend :: Trail -> Bool
isTrailend (Trail elevation) = elevation == 9

canMove :: Trail -> Trail -> Bool
canMove (Trail fromElevation) (Trail toElevation) = (toElevation - fromElevation) == 1

isSurrounding :: Position -> Position -> Bool
isSurrounding (x1, y1) (x2, y2) = 
       (x1 == x2 && (y1+1) == y2) 
    || (x1 == x2 && (y1-1) == y2) 
    || ((x1+1) == x2 && y1 == y2)
    || ((x1-1) == x2 && y1 == y2)

getSurroundingTrails :: TrailPos -> [TrailPos] -> [TrailPos]
getSurroundingTrails (pos, _) = filter (\(pos2, _) -> isSurrounding pos pos2)


hike :: TrailPos -> [TrailPos] -> Set.Set Position -> Set.Set Position
hike trailPos@(currPos, currTrail) trail endPositions = do
    case isTrailend currTrail of
        True -> Set.insert currPos endPositions
        False -> do
            let surrounding = getSurroundingTrails trailPos trail
            let canMoveToSurrounding = filter (canMove currTrail . snd) surrounding
            Set.unions $ map (\newStart -> hike newStart trail endPositions) canMoveToSurrounding

hikeAndTrack :: [TrailPos] -> [TrailPos] -> [[TrailPos]]
hikeAndTrack (currentTrail:tailTrails) trails = do
    case (isTrailend . snd) currentTrail of 
        True -> return (currentTrail:tailTrails)
        False -> do
            let surrounding = getSurroundingTrails currentTrail trails
            let canMoveToSurrounding = filter (canMove (snd currentTrail) . snd) surrounding
            concat $ map (\nextTrail -> hikeAndTrack (nextTrail:currentTrail:tailTrails) trails) canMoveToSurrounding      
    

solution1 :: String -> Int
solution1 input = do
    let trails = (map . map) (\v -> Trail (readInt (v:[]))) $ lines input 
    let posTrails = flatMatrix trails
    sum 
        . map Set.size 
        . map (\trailhead -> hike trailhead posTrails Set.empty) 
        . filter (isTrailhead . snd) 
        $ posTrails


solution2 :: String -> Int
solution2 input = do
    let trails = (map . map) (\v -> Trail (readInt (v:[]))) $ lines input 
    let posTrails = flatMatrix trails
    length 
        . concat 
        . map (\trailhead -> hikeAndTrack (trailhead:[]) posTrails) 
        . filter (isTrailhead . snd) 
        $ posTrails


main :: IO()
main = do
    input <- readFile "inputs/day10.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input