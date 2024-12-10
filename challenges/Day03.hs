{-# LANGUAGE InstanceSigs #-}
module Main where
import Control.Applicative (Alternative (many), (<|>), empty)
import Data.Char (isDigit)
import ChallengeUtils ( readInt, printSolution1, printSolution2, Parser (runParser, Parser), parseString, parseChar, parseSpan )

data Token = Mul Int Int | Gibberish | Do | Dont deriving (Show)

parseMul :: Parser Token
parseMul = Parser f 
    where
        f s = do 
            (remaining, firstNumber) <- runParser ((parseString "mul") *> (parseChar '(') *> (parseSpan isDigit)) s
            (remaining', secondNumber) <- runParser (parseChar ',' *> parseSpan isDigit <* parseChar ')') remaining
            Just (remaining', Mul (readInt firstNumber) (readInt secondNumber))

parseGibberish :: Parser Token
parseGibberish = Parser f 
    where
        f [] = Nothing
        f s = Just (tail s, Gibberish)

parseDo :: Parser Token
parseDo = (\_ -> Do) <$> parseString "do()"

parseDont :: Parser Token
parseDont = (\_ -> Dont) <$> parseString "don't()"

mult :: Token -> Int
mult (Mul a b) = a * b
mult _ = 0

conditionalMult :: Token -> Token -> Int
conditionalMult Do (Mul a b) = a * b
conditionalMult _ _ = 0

recursivelyMultiply :: Token -> [Token] -> [Int] -> (Token, [Token], [Int])
recursivelyMultiply _ [] mults = (Dont, [], mults)
recursivelyMultiply Do (headToken:tailTokens) mults = 
    case headToken of
        Do -> recursivelyMultiply Do tailTokens mults
        Mul a b -> recursivelyMultiply Do tailTokens ((a * b):mults)
        Dont -> recursivelyMultiply Dont tailTokens mults
        Gibberish -> recursivelyMultiply Do tailTokens mults
recursivelyMultiply Dont (headToken:tailTokens) mults = 
    case headToken of
        Do -> recursivelyMultiply Do tailTokens mults
        _ -> recursivelyMultiply Dont tailTokens mults
    

solution1 :: String -> Int
solution1 input = do
    let parsedInput = runParser (many (parseMul <|> parseGibberish)) input
    case parsedInput of 
        Nothing -> 0
        Just (_, tokens) -> (sum . (map mult)) tokens

solution2 :: String -> Int
solution2 input = do
    let parsedInput = runParser (many (parseMul <|> parseDo <|> parseDont <|> parseGibberish)) input
    case parsedInput of 
        Nothing -> 9
        Just (_, tokens) -> do
            let (_, _, mults) = recursivelyMultiply Do tokens []
            sum mults


main :: IO()
main = do
    input <- readFile "inputs/day03.txt"
    printSolution1 $ solution1 input
    printSolution2 $ solution2 input