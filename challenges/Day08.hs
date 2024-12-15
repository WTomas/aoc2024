module Main where

import ChallengeUtils (flatMatrix, Position, both, pairs, printSolution1, printSolution2)
import Data.Maybe (isJust)
import qualified Data.Set as Set

data MapTile = Tile | Antenna Char deriving (Show)

isAntenna :: MapTile -> Bool
isAntenna Tile = False
isAntenna (Antenna _) = True

extendLine :: Position -> Position -> (Position, Position)
extendLine (x1, y1) (x2, y2)
    | (x1 <= x2) && (y1 <= y2) = ((x1 - abs (x1 - x2), (y1 - abs (y1 - y2))), (x2 + abs (x1 - x2), y2 + abs (y1 - y2)))
    | (x1 <= x2) && (y1 >= y2) = ((x1 - abs (x1 - x2), (y1 + abs (y1 - y2))), (x2 + abs (x1 - x2), y2 - abs (y1 - y2)))
    | otherwise = extendLine (x2, y2) (x1, y1)

extendLine2 :: Int -> Int -> Position -> Position -> [Position]
extendLine2 maxX maxY (x1, y1) (x2, y2)
    | (x1 <= x2) && (y1 <= y2) = 
        let xDiff = abs (x1 - x2)
            yDiff = abs (y1 - y2)
        in zip (takeWhile (>= 0) $ map (\n -> x1 - xDiff * n) [0..]) (takeWhile (>= 0) $ map (\n -> y1 - yDiff * n) [0..])
            ++ zip (takeWhile (<= maxX) $ map (\n -> x2 + xDiff * n) [0..]) (takeWhile (<= maxY) $ map (\n -> y2 + yDiff * n) [0..])
    | (x1 <= x2) && (y1 >= y2) = 
        let xDiff = abs (x1 - x2)
            yDiff = abs (y1 - y2)
        in zip (takeWhile (>= 0) $ map (\n -> x1 - xDiff * n) [0..]) (takeWhile (<= maxY) $ map (\n -> y1 + yDiff * n) [0..])
            ++ zip (takeWhile (<= maxX) $ map (\n -> x2 + xDiff * n) [0..])  (takeWhile (>= 0) $ map (\n -> y2 - yDiff * n) [0..])
    | otherwise = extendLine2 maxX maxY (x2, y2) (x1, y1)
            
makeAntinodes :: (Position, MapTile) -> (Position, MapTile) -> Maybe (Position, Position)
makeAntinodes (_, Tile) _ = Nothing
makeAntinodes _ (_, Tile) = Nothing
makeAntinodes (p1, Antenna c1) (p2, Antenna c2)
    | c1 == c2 = Just $ extendLine p1 p2
    | otherwise = Nothing

makeAntinodes2 :: Int -> Int -> (Position, MapTile) -> (Position, MapTile) -> Maybe [Position]
makeAntinodes2 _ _ (_, Tile) _ = Nothing
makeAntinodes2 _ _ _ (_, Tile) = Nothing
makeAntinodes2 maxX maxY (p1, Antenna c1) (p2, Antenna c2)
    | c1 == c2 = Just $ extendLine2 maxX maxY p1 p2
    | otherwise = Nothing

withinIndices :: Int -> Int -> Position -> Bool
withinIndices width height (posX, posY) = posX >= 0 && posX <= width && posY >= 0 && posY <= height

solution1 :: String -> Int
solution1 input = do
    let mapTiles = flatMatrix $ map (map (\c -> if c == '.' then Tile else Antenna c)) $ lines input
    let mapWidth = maximum $ map (fst . fst) mapTiles  
    let mapHeight = maximum $ map (snd . fst) mapTiles  
    Set.size . Set.filter (withinIndices mapWidth mapHeight) $ Set.fromList $ concat . map (\(Just (a, b)) -> a:b:[]) $ filter isJust $ map (uncurry makeAntinodes) $ pairs $ filter (isAntenna . snd) mapTiles

solution2 :: String -> Int
solution2 input = do
    let mapTiles = flatMatrix $ map (map (\c -> if c == '.' then Tile else Antenna c)) $ lines input
    let maxX = maximum $ map (fst . fst) mapTiles  
    let maxY = maximum $ map (snd . fst) mapTiles  
    Set.size . Set.fromList . concat . map (\(Just x) -> x) $ filter isJust $ map (uncurry (makeAntinodes2 maxX maxY)) $ pairs $ filter (isAntenna . snd) mapTiles

main :: IO()
main = do
    input <- readFile "inputs/day08.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input