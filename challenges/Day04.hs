module Main where
  
import Data.List (transpose, intercalate)
import Utils (enumerate, flatMatrix, Position, splitEvery, boolToInt, printSolution1)

newtype Mask = Mask [[Char]]

instance Show Mask where
  show (Mask mask) = intercalate "\n" mask

masks :: [Mask]
masks = [
    (Mask ["XMAS", "....", "....", "...."])
  , (Mask ["SAMX", "....", "....", "...."])
  , (Mask ["X...", "M...", "A...", "S..."])
  , (Mask ["S...", "A...", "M...", "X..."])
  , (Mask ["X...", ".M..", "..A.", "...S"])
  , (Mask ["S...", ".A..", "..M.", "...X"])
  , (Mask ["...X", "..M.", ".A..", "S..."])
  , (Mask ["...S", "..A.", ".M..", "X..."])
  ]

padInput :: [[Char]] -> [[Char]]
padInput input = do
  let widthPaddedInput = map (\chars -> chars ++ "...") input 
  widthPaddedInput ++ (take 3 $ repeat $ take (length (widthPaddedInput !! 1)) $ repeat '.')
  
subsection :: Int -> Position -> [[Char]] -> [[Char]]
subsection window (posX, posY) input = (splitEvery window) . (map snd) $ filter (\((x, y), _) -> x >= posX && x < posX + window && y >= posY && y < posY + window) (flatMatrix input)

isMasked :: Mask -> [[Char]] -> Bool
isMasked (Mask mask) subsection = all id $ map (\(maskC, c) -> maskC == '.' || maskC == c) $ zip (concat mask) (concat subsection)

countFittingMasks :: [Mask] -> [[Char]] -> Int
countFittingMasks masks subsection = sum $ map boolToInt $ map ((flip isMasked) subsection) masks

solution1 :: String -> Int
solution1 input = do
  let inputLines = lines input
  let xDim = length $ inputLines !! 0
  let yDim = length inputLines
  let positions = (,) <$> [0..xDim] <*> [0..yDim]
  sum $ map (\position -> countFittingMasks masks $ subsection 4 position (padInput inputLines)) positions

main :: IO()
main = do
  input <- readFile "inputs/day04.txt"
  printSolution1 $ solution1 input
