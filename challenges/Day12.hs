module Main where

import ChallengeUtils (flatMatrix, Position, fork, printSolution1, printSolution2, findAndPop, Counter (_map))
import qualified Data.Set as S

data Flower = Flower Char deriving (Show, Eq)
type PosFlower = (Position, Flower)
data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq, Ord)

getNeighbourPositions :: Position -> [Position]
getNeighbourPositions (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

getNeighbourPositionsWithDirections :: Position -> [(Position, Direction)]
getNeighbourPositionsWithDirections pos = zip (getNeighbourPositions pos) [RIGHT, LEFT, UP, DOWN]

popNeighbours :: PosFlower -> [PosFlower] -> ([PosFlower], [PosFlower])
popNeighbours (pos, flower) land = (filter predicate land, filter (not . predicate) land)
    where 
        predicate :: PosFlower -> Bool
        predicate (pos', flower') = flower == flower' && pos' `elem` getNeighbourPositions pos

type GrowFunction = [PosFlower] -> [PosFlower] -> S.Set Position -> ([PosFlower], [PosFlower], S.Set Position)

startGrowing :: [PosFlower] -> ([PosFlower], [PosFlower], GrowFunction)
startGrowing (current:restOfLand) = (restOfLand, [current], growPlot)
    where 
        growPlot :: GrowFunction
        growPlot [] queue plot = ([], queue, S.union plot ((S.fromList  . map fst) queue))
        growPlot land [] plot = (land, [], plot)
        growPlot land (firstFlower@(pos, flower):restOfFlowers) plot = do
            let (neighbours, remainingLand) = popNeighbours firstFlower land
            growPlot remainingLand (restOfFlowers ++ neighbours) (S.insert  pos plot)


consumeLand :: [PosFlower] -> [S.Set Position]
consumeLand land = consumeLand' land []
    where 
        consumeLand' :: [PosFlower] -> [S.Set Position] -> [S.Set Position]
        consumeLand' [] acc = acc
        consumeLand' land acc = do
            let (remainingLand, queue, growF) = startGrowing land
            let (remainingLand', _, plot) = growF remainingLand queue S.empty
            consumeLand' remainingLand' (plot:acc)

getPerimeter :: S.Set Position -> Int
getPerimeter plot = do
    let surroundingPositions = concat . (map getNeighbourPositions) $ S.toList plot
    length $ filter (not . (flip S.member) plot) surroundingPositions


mergeSides :: [(Position, Direction)] -> [S.Set (Position, Direction)]
mergeSides xs = mergeSides' xs []
    where
        mergeSides' :: [(Position, Direction)] -> [S.Set (Position, Direction)] -> [S.Set (Position, Direction)]
        mergeSides' [] acc = acc
        mergeSides' (side:restOfSides) acc = do
            let (acc', maybeSides) = findAndPop (canBeMerged side) acc
            case maybeSides of
                Just sides -> mergeSides' restOfSides ((S.insert side sides):acc' )
                Nothing -> mergeSides' restOfSides ((S.insert side S.empty):acc')

        canBeMerged :: (Position, Direction) -> S.Set (Position, Direction) -> Bool
        canBeMerged (pos, dir) sides = do
            let neighbouringSides = map (\p -> (p, dir)) $ getNeighbourPositions pos
            any (\neighbouringSide -> S.member neighbouringSide sides) neighbouringSides


getSides :: S.Set Position -> Int
getSides plot = do
    let surroundingPositionsWithDirections = concat . (map getNeighbourPositionsWithDirections) $ S.toList plot
    length . mergeSides . filter (not . (flip S.member) plot . fst) $ surroundingPositionsWithDirections


solution1 :: String -> Int
solution1 input = do
    let land = flatMatrix $  (map . map) Flower $ lines input
    let plots =  consumeLand land
    sum 
        . map (uncurry (*)) 
        . map (fork getPerimeter S.size) 
        $ plots


solution2 :: String -> Int
solution2 input = do
    let land = flatMatrix $  (map . map) Flower $ lines input
    let plots =  consumeLand land
    sum 
        . map (uncurry (*)) 
        . map (fork getSides S.size) 
        $ plots
        

main :: IO()
main = do
    input <- readFile "inputs/day12.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input
