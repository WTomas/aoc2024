module Main where 

import ChallengeUtils (Parser (runParser), parseChar, Position, flatMatrix, uncurry3, get3rd, printSolution1)
import Control.Applicative (Alternative(many), (<|>))
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import qualified Data.Set as Set

data Direction = LEFT | RIGHT | UP | DOWN deriving (Show)

data Guard = Guard Direction deriving Show

data Tile = 
      Pathway 
    | Obstruction 
    | Edge
    deriving (Show)

data GameTile = Tile Tile | TileGuard Guard deriving (Show)

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

makeMove :: [(Position, Tile)] -> (Position, Guard) -> ((Position, Guard), Bool)
makeMove tiles guard = do
  let ((nextGuardX, nextGuardY), nextGuard) = guardNextPosition guard
  let (_, tile) = fromMaybe ((nextGuardX, nextGuardY), Edge) $ find (\((tileX, tileY), _) -> tileX == nextGuardX && tileY == nextGuardY) tiles
  case tile of
    Edge -> (((nextGuardX, nextGuardY), nextGuard), True)
    Pathway -> (((nextGuardX, nextGuardY), nextGuard), False)
    Obstruction -> ((fst guard, (turnGuard . snd) guard), False)


makeMoves :: [(Position, Tile)] -> (Position, Guard) -> Set.Set Position -> ([(Position, Tile)], (Position, Guard), Set.Set Position)
makeMoves tiles guard path = do
  let updatedPath = (Set.insert . fst) guard path
  let (nextGuard, hasLeft) = makeMove tiles guard
  case hasLeft of
    True -> (tiles, nextGuard, updatedPath)
    False -> makeMoves tiles nextGuard updatedPath 

solution1 :: String -> Int
solution1 input = do
    let tiles = sequence $ map ((<$>) snd) $ map (runParser (many (parseObstruction <|> parsePathway <|> parseGuard))) $ lines input
    case tiles of 
      Nothing -> error "No map"
      Just tiles' -> do 
        let (tiles, guard) = extractGuard $ flatMatrix $ padTiles tiles'
        Set.size $ get3rd $ makeMoves tiles guard Set.empty
        

main :: IO()
main = do
  input <- readFile "inputs/day06.txt"
  printSolution1 $ solution1 input
