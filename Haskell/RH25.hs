#! /usr/bin/runhaskell

import System.Environment
import Fasta (readFastaFile, recordSequence)

main :: IO ()
main = do
        args <- getArgs
        fasta <- if length args > 0 then readFastaFile (head args) else return []
        sequence <- return (recordSequence (head fasta))
        print (perfectMatches (stringToNucPos sequence))

data Nucleotide = A | U | C | G
    deriving (Eq, Show)
data NucPos = NucPos Int Nucleotide
    deriving (Eq, Show)

stringToNucPos :: String -> [NucPos]
stringToNucPos = stringToNucPos' 1
    where
    stringToNucPos' :: Int -> String -> [NucPos]
    stringToNucPos' _ [] = [] 
    stringToNucPos' n x =
        case x of
            ('A':xs) -> (NucPos n A):(stringToNucPos' (n+1) xs)
            ('C':xs) -> (NucPos n C):(stringToNucPos' (n+1) xs)
            ('U':xs) -> (NucPos n U):(stringToNucPos' (n+1) xs)
            ('G':xs) -> (NucPos n G):(stringToNucPos' (n+1) xs)
            (_:xs)   -> stringToNucPos' n xs

bond :: NucPos -> NucPos -> Bool
bond (NucPos m b) (NucPos n c)
    | m + 1 == n    = True
    | b == U        = c == A
    | b == A        = c == U
    | b == G        = c == C
    | b == C        = c == G
    | otherwise     = False

perfectMatches :: [NucPos] -> Int
perfectMatches [] = 1
perfectMatches [x, y] = if bond x y then 1 else 0
perfectMatches (x:xs) = sum (map match xs)
    where
    match :: NucPos -> Int
    match y
        | bond x y    = perfectMatches (xs `remove` y)
        | otherwise   = 0


remove :: (Eq a) => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) y
    | x == y    = xs
    | otherwise = x: (remove xs y)
