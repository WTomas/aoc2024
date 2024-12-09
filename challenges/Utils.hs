{-# LANGUAGE NamedFieldPuns #-}

module Utils where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (deleteBy)

printSolution:: (Show a) => Int ->  a -> IO()
printSolution n solution = putStrLn $ "Solution " ++ show n ++ ": "  ++ show solution

printSolution1:: (Show a) => a -> IO()
printSolution1 = printSolution 1

printSolution2:: (Show a) => a -> IO()
printSolution2 = printSolution 2

readInt :: String -> Int
readInt = read

data (Ord a) => Counter a = Counter {
    _map :: M.Map a Int 
}

(!) :: (Ord a) => Counter a -> a -> Int
(Counter { _map }) ! element = fromMaybe 0 $ element `M.lookup` _map

incrementCounter :: (Ord a) => Counter a -> a -> Counter a
incrementCounter (counter@Counter { _map }) element = Counter { _map = M.insert element (counter ! element + 1) _map }

makeCounter :: (Ord a) => [a] -> Counter a
makeCounter as = foldl incrementCounter Counter { _map = M.empty } as

instance (Ord a, Show a) => Show (Counter a) where
    show (Counter { _map }) = show _map

deleteIndex :: Int -> [a] -> [a]
deleteIndex idx arr = map snd $ filter ( (/=) idx . fst) $ zip [0..] arr

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

type Position = (Int, Int)

flatMatrix :: [[a]] -> [(Position, a)]
flatMatrix values = concat $ map (\(yIndex, withX) -> map (\(xIndex, value) -> ((xIndex, yIndex), value)) withX) $ enumerate $ map enumerate values

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list


boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf l1 l2 = any (\(v1) -> any (\(v2) -> v1 == v2) l2) l1

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z 