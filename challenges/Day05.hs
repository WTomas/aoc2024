module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import ChallengeUtils (readInt, enumerate, isSubsetOf, printSolution1, printSolution2, uncurry3, get3rd)
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

reorderPageNumbers :: Mapping -> [Int] -> [Int]
reorderPageNumbers mapping pageNumbers = get3rd $ reorderPageNumbers' mapping pageNumbers []
    where 
        reorderPageNumbers' :: Mapping -> [Int] -> [Int] -> (Mapping, [Int], [Int])
        reorderPageNumbers' mapping [] reordered = (mapping, [], reordered)
        reorderPageNumbers' mapping (pageNumber:pageNumbers) reordered = uncurry3 reorderPageNumbers' (mapping, pageNumbers, insertElement mapping pageNumber reordered)

        insertElement :: Mapping -> Int -> [Int] -> [Int]
        insertElement mapping pageNumber reorderedPageNumbers = do
            let mustBeBefore = fromMaybe [] $ M.lookup pageNumber mapping
            let boolValues = scanr1 (||) $ map ((flip elem) mustBeBefore) reorderedPageNumbers
            let (before, after) = groupElements $ zip boolValues reorderedPageNumbers
            before ++ [pageNumber] ++ after

        groupElements :: [(Bool, a)] -> ([a], [a])
        groupElements = foldl (\(trueValues, falseValues) (b, value) -> if b then (trueValues ++ [value], falseValues) else (trueValues, falseValues ++ [value])) ([], [])

solution1 :: String -> Int
solution1 input = do
    let (mapping, pageNumbers) = parseInput input
    sum $ map getMiddleValue $ filter (arePageNumbersCorrect mapping) pageNumbers

solution2 :: String -> Int
solution2 input = do
    let (mapping, pageNumbers) = parseInput input
    let incorrectPageNumbers = filter (not . arePageNumbersCorrect mapping) pageNumbers
    -- sum $ map getMiddleValue $ map (reorderPageNumbers mapping) incorrectPageNumbers
    sum $ map getMiddleValue $ map (reorderPageNumbers mapping) incorrectPageNumbers

main :: IO()
main = do
    input <- readFile "inputs/day05.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input