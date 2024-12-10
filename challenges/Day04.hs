module Main where
  
import Data.List (transpose, intercalate)
import ChallengeUtils (enumerate, flatMatrix, Position, splitEvery, boolToInt, printSolution1, printSolution2)

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

masks2 :: [Mask]
masks2 = [
    (Mask ["M.M", ".A.", "S.S"])
  , (Mask ["M.S", ".A.", "M.S"])
  , (Mask ["S.M", ".A.", "S.M"])
  , (Mask ["S.S", ".A.", "M.M"])
  ]


padInput :: Int -> [[Char]] -> [[Char]]
padInput n input = do
  let widthPaddedInput = map (\chars -> chars ++ take n (repeat '.')) input 
  widthPaddedInput ++ (take n $ repeat $ take (length (widthPaddedInput !! 1)) $ repeat '.')
  
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
  sum $ map (\position -> countFittingMasks masks $ subsection 4 position (padInput 3 inputLines)) positions

solution2 :: String -> Int
solution2 input = do
  let inputLines = lines input
  let xDim = length $ inputLines !! 0
  let yDim = length inputLines
  let positions = (,) <$> [0..xDim] <*> [0..yDim]
  sum $ map (\position -> countFittingMasks masks2 $ subsection 3 position (padInput 2 inputLines)) positions

main :: IO()
main = do
  input <- readFile "inputs/day04.txt"
  printSolution1 $ solution1 input
  printSolution2 $ solution2 input
