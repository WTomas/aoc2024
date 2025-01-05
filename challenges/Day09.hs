module Main where

import ChallengeUtils (readInt, replaceAt, printSolution1, printSolution2)
import Data.Char (digitToInt)
import Data.Foldable (find)
import Data.List (findIndex, groupBy)
import Data.Maybe (fromMaybe)

data DiskAllocation = FreeSpace | File Int deriving (Show)
data DiskSpace = DiskSpace Int [DiskAllocation] deriving (Show)

isFile :: DiskAllocation -> Bool
isFile FreeSpace = False
isFile (File _) = True

calculateFreeSpace :: DiskSpace -> Int
calculateFreeSpace (DiskSpace _ allocation) = length $ filter (not . isFile) allocation 

calculateFileSize :: DiskSpace -> Int
calculateFileSize (DiskSpace _ allocation) = length $ filter isFile allocation 

isDiskSpaceFull :: DiskSpace -> Bool
isDiskSpaceFull = (==) 0 . calculateFreeSpace

isDiskSpaceEmpty :: DiskSpace -> Bool
isDiskSpaceEmpty diskSpace@(DiskSpace size allocation) = size == calculateFreeSpace diskSpace

getFileId :: DiskSpace -> Maybe Int
getFileId (DiskSpace _ allocation) = (\(File fileId) -> fileId) <$> find isFile allocation

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

transferFile :: DiskSpace -> DiskSpace -> (DiskSpace, DiskSpace)
transferFile fromDiskSpace toDiskSpace
    | calculateFreeSpace toDiskSpace >= calculateFileSize fromDiskSpace = do
        iterate (uncurry transferFileAllocation) (fromDiskSpace, toDiskSpace) !! (calculateFileSize fromDiskSpace)
    | otherwise = (fromDiskSpace, toDiskSpace) 

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

transferFiles :: [DiskSpace] -> [DiskSpace]
transferFiles diskSpaces = foldr f diskSpaces diskSpaces
    where 
        f :: DiskSpace -> [DiskSpace] -> [DiskSpace]
        f diskSpace diskSpaces'
            | isDiskSpaceEmpty diskSpace = diskSpaces'
            | otherwise = do
                let freeSpaceIdx = fromMaybe (-1) $ findIndex (\ds -> calculateFreeSpace ds >= calculateFileSize diskSpace) diskSpaces'
                let fileSpaceIdx = fromMaybe (-2) $ findIndex (\ds -> getFileId ds == getFileId diskSpace) diskSpaces'
                case freeSpaceIdx < 0 of 
                    True -> diskSpaces'
                    False -> do
                        case fileSpaceIdx < 0 of 
                            True -> diskSpaces'
                            False -> do
                                case freeSpaceIdx < fileSpaceIdx of
                                    True -> let (from', to') = transferFile diskSpace (diskSpaces' !! freeSpaceIdx)
                                        in (replaceAt freeSpaceIdx to') . (replaceAt fileSpaceIdx from') $ diskSpaces'
                                    False -> diskSpaces'
    
consumeDiskMap :: Int -> Bool -> [Int] -> [DiskSpace] -> (Int, Bool, [Int], [DiskSpace])
consumeDiskMap idNumber isFile [] diskSpace = (idNumber, isFile, [], reverse diskSpace)
consumeDiskMap idNumber True (d:diskMap) diskSpace = consumeDiskMap (idNumber+1) False diskMap ((DiskSpace d (take d $ repeat (File idNumber))):diskSpace)
consumeDiskMap idNumber False (d:diskMap) diskSpace = consumeDiskMap idNumber True diskMap ((DiskSpace d (take d $ repeat FreeSpace)):diskSpace)

checkSum :: [DiskSpace] -> Int
checkSum  = sum 
    . map (uncurry (*)) 
    . zip [0..] 
    . map allocationValue
    . concat 
    . map (\(DiskSpace _ allocation) -> allocation)
    where 
        allocationValue :: DiskAllocation -> Int
        allocationValue FreeSpace = 0
        allocationValue (File x) = x

    
solution1 :: String -> Int
solution1 input = do
    let diskMap = map digitToInt input 
    let (_, _, _, diskSpace) = consumeDiskMap 0 True diskMap []
    checkSum $ transfer diskSpace

solution2 :: String -> Int
solution2 input = do
    let diskMap = map digitToInt input 
    let (_, _, _, diskSpace) = consumeDiskMap 0 True diskMap []
    checkSum $ transferFiles diskSpace


main :: IO()
main = do
    input <- readFile "inputs/day09.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input