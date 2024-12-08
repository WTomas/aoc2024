module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import Utils (readInt, enumerate, isSubsetOf, printSolution1)
import Data.Maybe (fromMaybe)

type Mapping = M.Map Int [Int]

-- Creates reverse mappings, i.e. values that must appear *before* the key
makeMapping :: [String] -> Mapping
makeMapping mappingLines = foldl makeMapping' M.empty mappingLines
    where 
        makeMapping' :: Mapping -> String -> Mapping
        makeMapping' mapping line = do
            let a:b:[] = map (readInt . T.unpack) $ T.splitOn (T.pack "|") (T.pack line)
            let existingValues = fromMaybe [] (M.lookup b mapping)
            M.insert b (a:existingValues) mapping
    
makePageNumbers :: [String] -> [[Int]]
makePageNumbers = map ((map (readInt . T.unpack)) . (T.splitOn (T.pack ",")) . T.pack)

parseInput :: String -> (Mapping, [[Int]])
parseInput input = do
    let mappingLines:pageNumberLines:[] = map T.unpack $ ((T.splitOn (T.pack "\n\n")) . T.pack) input
    ((makeMapping . lines) mappingLines, (makePageNumbers . lines) pageNumberLines)

arePageNumbersCorrect :: Mapping -> [Int] -> Bool
arePageNumbersCorrect mapping pageNumbers = do
    let enumeratedPageNumbers = enumerate pageNumbers
    all (isPageNumberCorrect enumeratedPageNumbers) enumeratedPageNumbers
    where 
        isPageNumberCorrect :: [(Int, Int)] -> (Int, Int) -> Bool
        isPageNumberCorrect enumeratedPageNumbers (index, pageNumber) = do
            let trailingPageNumbers = map snd $ filter (\(i, _) -> i > index) enumeratedPageNumbers
            let constrainedPageNumbers = fromMaybe [] $ M.lookup pageNumber mapping 
            not $ isSubsetOf constrainedPageNumbers trailingPageNumbers

getMiddleValue :: [Int] -> Int
getMiddleValue [] = 0
getMiddleValue values = values !! (length values `div` 2)

solution1 :: String -> Int
solution1 input = do
    let (mapping, pageNumbers) = parseInput input
    sum $ map getMiddleValue $ filter (arePageNumbersCorrect mapping) pageNumbers

main :: IO()
main = do
    input <- readFile "inputs/day05.txt"
    printSolution1 $ solution1 input