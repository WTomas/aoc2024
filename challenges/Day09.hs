module Main where

import ChallengeUtils (readInt, replaceAt, printSolution1)
import Data.Char (digitToInt)
import Data.Foldable (find)
import Data.List (findIndex, groupBy)
import Data.Maybe (fromMaybe)

data DiskAllocation = FreeSpace | File Int deriving (Show)
data DiskSpace = DiskSpace Int [DiskAllocation] deriving (Show)

isFile :: DiskAllocation -> Bool
isFile FreeSpace = False
isFile (File _) = True

isDiskSpaceFull :: DiskSpace -> Bool
isDiskSpaceFull (DiskSpace size allocation) = size == (length $ filter isFile allocation)

isDiskSpaceEmpty :: DiskSpace -> Bool
isDiskSpaceEmpty (DiskSpace size allocation) = size == (length $ filter (not . isFile) allocation)

removeFileAllocation :: DiskSpace -> (DiskSpace, DiskAllocation)
removeFileAllocation (DiskSpace size allocations) = do 
    let reverseFileIndex = findIndex isFile (reverse allocations)
    case reverseFileIndex of
        Nothing -> (DiskSpace size allocations, FreeSpace)
        Just reverseFileIndex' -> do
            let fileIndex = (length allocations) - 1 - reverseFileIndex'
            (DiskSpace size ((map (\(idx, a) -> if idx == fileIndex then FreeSpace else a)) (zip [0..] allocations)), allocations !! fileIndex)

addFileAllocation :: DiskAllocation -> DiskSpace -> DiskSpace
addFileAllocation file (DiskSpace size allocations) = do
    let fileAllocations = filter isFile allocations
    let _:remainingEmptyAllocations = filter (not . isFile) allocations
    (DiskSpace size (fileAllocations ++ [file] ++ remainingEmptyAllocations))


transferFileAllocation :: DiskSpace -> DiskSpace -> (DiskSpace, DiskSpace)
transferFileAllocation fromDiskSpace toDiskSpace = do
    let (fromDiskSpace', file) = removeFileAllocation fromDiskSpace
    let toDiskSpace' = addFileAllocation file toDiskSpace
    (fromDiskSpace', toDiskSpace')

orderFileAllocation :: DiskSpace -> DiskSpace
orderFileAllocation (DiskSpace size allocation) = DiskSpace size ((filter isFile allocation) ++ (filter (not . isFile) allocation))

transfer :: [DiskSpace] -> [DiskSpace]
transfer diskSpaces = do
    let (freeSpaceIndex, freeSpace) = fromMaybe (-1, DiskSpace 0 []) $ find (not . isDiskSpaceFull . snd) $ zip [0..] diskSpaces
    let (nonEmptySpaceIndex, nonEmptySpace) = fromMaybe (-1, DiskSpace 0 []) $ find (not . isDiskSpaceEmpty . snd) $ reverse $ zip [0..] diskSpaces
    if freeSpaceIndex == nonEmptySpaceIndex 
        then replaceAt freeSpaceIndex (orderFileAllocation freeSpace) diskSpaces 
        else case freeSpaceIndex < nonEmptySpaceIndex of
            False -> diskSpaces
            True -> do
                let (newNonEmptySpace, newFreeSpace) = transferFileAllocation nonEmptySpace freeSpace
                transfer . (replaceAt nonEmptySpaceIndex newNonEmptySpace) . (replaceAt freeSpaceIndex newFreeSpace) $ diskSpaces
    
consumeDiskMap :: Int -> Bool -> [Int] -> [DiskSpace] -> (Int, Bool, [Int], [DiskSpace])
consumeDiskMap idNumber isFile [] diskSpace = (idNumber, isFile, [], reverse diskSpace)
consumeDiskMap idNumber True (d:diskMap) diskSpace = consumeDiskMap (idNumber+1) False diskMap ((DiskSpace d (take d $ repeat (File idNumber))):diskSpace)
consumeDiskMap idNumber False (d:diskMap) diskSpace = consumeDiskMap idNumber True diskMap ((DiskSpace d (take d $ repeat FreeSpace)):diskSpace)

checkSum :: [DiskSpace] -> Int
checkSum diskSpace = sum 
    $ map (uncurry (*)) 
    $ zip [0..] $ map (\(File idNumber) -> idNumber) 
    $ filter isFile 
    $ concat 
    $ map (\(DiskSpace _ allocation) -> allocation) diskSpace

solution1 :: String -> Int
solution1 input = do
    let diskMap = map digitToInt input 
    let (_, _, _, diskSpace) = consumeDiskMap 0 True diskMap []
    checkSum $ transfer diskSpace

main :: IO()
main = do
    input <- readFile "inputs/day09.txt"
    printSolution1 $ solution1 input
