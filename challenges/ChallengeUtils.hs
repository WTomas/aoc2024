{-# LANGUAGE NamedFieldPuns #-}

module ChallengeUtils where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (deleteBy)
import Control.Applicative (Alternative (empty), (<|>))

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

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
     fmap f (Parser g) = Parser h
        where
            h s = do
                (remaining, x) <- g s
                Just (remaining, f x)

instance Applicative Parser where
    Parser f <*> Parser a = Parser g
        where 
            g s = do
                (remaining, f') <- f s
                (remaining', a') <- a remaining
                Just (remaining', f' a')
    pure x = Parser h
        where 
            h s = Just (s, x)

instance Alternative Parser where
    (Parser a) <|> (Parser b) = Parser f
        where f s = a s <|> b s
    empty = Parser (\_ -> Nothing)

parsePredicate :: (Char -> Bool) -> Parser Char
parsePredicate predicate = Parser f where
    f (x:xs)
        | predicate x = Just (xs, x)
        | otherwise = Nothing
    f [] = Nothing

parseChar :: Char -> Parser Char
parseChar c = parsePredicate (\input -> input == c)

parseString :: String -> Parser String
parseString = sequenceA . map parseChar

parseSpan :: (Char -> Bool) -> Parser String
parseSpan predicate = Parser f
    where
        f s = do
            let (spanned, remaining) = (span predicate) s  
            if spanned == [] then Nothing else Just (remaining, spanned)

get3rd :: (a, b, c) -> c
get3rd (_,_,x) = x