#! /usr/bin/runhaskell

-- calculate the longest decreasing & increasing subsequences of a permutation sequence of natural numbers
-- http://rosalind.info/problems/lgis/

import System.Environment
import Data.Map (Map)
import qualified Data.Map as Map (insert, lookup, empty)

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        sequence <- return (map read (words ((lines contents) !! 1))::[Int])
        putStrLn (printIntList (longestIncreasingSubsequence sequence))
        putStrLn (printIntList (longestDecreasingSubsequence sequence))

printIntList :: [Int] -> String
printIntList [] = []
printIntList [x] = show x
printIntList (x:xs) = show x ++ " " ++ printIntList xs

longestIncreasingSubsequence :: (Ord a) => [a] ->  [a]
longestIncreasingSubsequence = longestRelationalSubsequence (<)

longestDecreasingSubsequence :: (Ord a) => [a] ->  [a]
longestDecreasingSubsequence = longestRelationalSubsequence (>)

longestRelationalSubsequence :: (Ord a) => (a -> a -> Bool) -> [a] ->  [a]
longestRelationalSubsequence relation = reverse . (longestRelationalSubsequence' relation)
    where
    longestRelationalSubsequence' :: (Ord a) => (a -> a -> Bool) -> [a] ->  [a]
    longestRelationalSubsequence' _ []  = []
    longestRelationalSubsequence' relation seq = patience seq Map.empty []
        where
        patience [] m piles = let mapFollow n =
                                   case Map.lookup n m of
                                            Just x -> n:(mapFollow x)
                                            Nothing -> [n]
                                   in mapFollow (head (last piles))
        patience (x:xs) m piles = patience xs newMap newPiles
            where
            (newMap, newPiles) = place x piles
            place x [] = (m, [[x]])
            place n (x:xs)
                | relation n (head x)    = (m, (n:x):(xs))
                | otherwise              = let pair =
                                                place' (head x) xs in (fst pair, x:(snd pair))
                where
                place' x [] = (Map.insert n x m, [[n]])
                place' x (y:ys)
                    | relation n (head y)    = (Map.insert n x m, (n:y):ys)
                    | otherwise              = let next =
                                                    place' (head y) ys in (fst next, y:(snd next)) 
