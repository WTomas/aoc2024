module Main where 

import ChallengeUtils (Parser (runParser), parseChar, Position, flatMatrix, uncurry3, get3rd, printSolution1, printSolution2)
import Control.Applicative (Alternative(many), (<|>))
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import qualified Data.Set as Set

data Direction = LEFT | RIGHT | UP | DOWN deriving (Show, Eq, Ord)

data Guard = Guard Direction deriving Show

data Tile = 
      Pathway 
    | Obstruction 
    | Edge
    deriving (Show)

data GameTile = Tile Tile | TileGuard Guard deriving (Show)

data MoveResult = OnPath | LeftTiles | ReachedLoop deriving (Show, Eq)

type Path = Set.Set (Position, Direction)

parsePathway :: Parser GameTile
parsePathway = (\_ -> Tile Pathway) <$> parseChar '.'

parseObstruction :: Parser GameTile
parseObstruction = (\_ -> Tile Obstruction) <$> parseChar '#'

parseGuard :: Parser GameTile
parseGuard = 
      (\_ -> TileGuard (Guard RIGHT)) <$> parseChar '>'
  <|> (\_ -> TileGuard (Guard LEFT)) <$> parseChar '<'
  <|> (\_ -> TileGuard (Guard UP)) <$> parseChar '^'
  <|> (\_ -> TileGuard (Guard DOWN)) <$> parseChar 'v'

turnGuard :: Guard -> Guard
turnGuard (Guard UP) = Guard RIGHT
turnGuard (Guard RIGHT) = Guard DOWN
turnGuard (Guard DOWN) = Guard LEFT
turnGuard (Guard LEFT) = Guard UP

guardNextPosition :: (Position, Guard) -> (Position, Guard)
guardNextPosition ((x, y), Guard UP) = ((x, y-1), Guard UP)
guardNextPosition ((x, y), Guard DOWN) = ((x, y+1), Guard DOWN)
guardNextPosition ((x, y), Guard LEFT) = ((x-1, y), Guard LEFT)
guardNextPosition ((x, y), Guard RIGHT) = ((x+1, y), Guard RIGHT)

padTiles :: [[GameTile]] -> [[GameTile]]
padTiles tiles = do
  let widthPaddedTiles = map (\tiles' -> [Tile Edge] ++ tiles' ++ [Tile Edge]) tiles
  let topAndBottomEdges = take (length (widthPaddedTiles !! 1)) $ repeat (Tile Edge)
  [topAndBottomEdges] ++ widthPaddedTiles ++ [topAndBottomEdges]

extractGuard :: [(Position, GameTile)] -> ([(Position, Tile)], (Position, Guard))
extractGuard gameTiles = do
  let (guardPos, TileGuard (Guard direction)) = fromMaybe ((0, 0), TileGuard (Guard DOWN)) $ find isGuard gameTiles
  (map patchTiles gameTiles, (guardPos, Guard direction))
  where
    patchTiles :: (Position, GameTile) -> (Position, Tile)
    patchTiles (pos, TileGuard _) = (pos, Pathway)
    patchTiles (pos, Tile tile) = (pos, tile)

    isGuard :: (Position, GameTile) -> Bool
    isGuard (_, TileGuard _) = True
    isGuard _ = False

makeMove :: Path -> [(Position, Tile)] -> (Position, Guard) -> ((Position, Guard), MoveResult)
makeMove path tiles guard = do
  let ((nextGuardX, nextGuardY), (Guard nextDirection)) = guardNextPosition guard
  let (_, tile) = fromMaybe ((nextGuardX, nextGuardY), Edge) $ find (\((tileX, tileY), _) -> tileX == nextGuardX && tileY == nextGuardY) tiles
  case tile of
    Edge -> (((nextGuardX, nextGuardY), (Guard nextDirection)), LeftTiles)
    Pathway -> (((nextGuardX, nextGuardY), (Guard nextDirection)), (if Set.member ((nextGuardX, nextGuardY), nextDirection) path then ReachedLoop else OnPath))
    Obstruction -> makeMove path tiles (fst guard, (turnGuard . snd) guard)


makeMoves :: [(Position, Tile)] -> (Position, Guard) -> Path -> ([(Position, Tile)], (Position, Guard), Path, MoveResult)
makeMoves tiles (guardPosition, Guard direction) path = do
  let updatedPath = Set.insert (guardPosition, direction)  path
  let (nextGuard, moveResult) = makeMove path tiles (guardPosition, Guard direction)
  case moveResult of
    OnPath -> makeMoves tiles nextGuard updatedPath
    finalResult -> (tiles, nextGuard, updatedPath, finalResult)

placeObstruction :: [(Position, Tile)] -> Position -> [(Position, Tile)]
placeObstruction tiles position = map (placeObstruction' position) tiles
  where
    placeObstruction' :: Position -> (Position, Tile) -> (Position, Tile)
    placeObstruction' (posX, posY) tile = case tile of 
      (pos, Edge) -> (pos, Edge)
      ((tilePosX, tilePosY), tileType) -> if tilePosX == posX && tilePosY == posY then ((tilePosX, tilePosY), Obstruction) else ((tilePosX, tilePosY), tileType)

solution1 :: String -> Int
solution1 input = do
    let tiles = sequence $ map ((<$>) snd) $ map (runParser (many (parseObstruction <|> parsePathway <|> parseGuard))) $ lines input
    case tiles of 
      Nothing -> error "No map"
      Just tiles' -> do 
        let (tiles, guard) = extractGuard $ flatMatrix $ padTiles tiles'
        Set.size . Set.map fst . (\(_, _, path, moveResult) -> path) $ makeMoves tiles guard Set.empty

solution2 :: String -> Int
solution2 input = do
    let tiles = sequence $ map ((<$>) snd) $ map (runParser (many (parseObstruction <|> parsePathway <|> parseGuard))) $ lines input
    case tiles of 
      Nothing -> error "No map"
      Just tiles' -> do 
        let (tiles, guard) = extractGuard $ flatMatrix $ padTiles tiles'
        let guardPath = (\(_, _, path, moveResult) -> path) $ makeMoves tiles guard Set.empty
        length . filter (\moveResult -> moveResult == ReachedLoop) . map (\(_, _, _, result) -> result) . map (\(tilesWithObstruction) -> makeMoves tilesWithObstruction guard Set.empty ) . map (placeObstruction tiles) $ Set.toList $ Set.map fst guardPath
        
main :: IO()
main = do
  input <- readFile "inputs/day06.txt"
  printSolution1 $ solution1 input
  printSolution2 $ solution2 input