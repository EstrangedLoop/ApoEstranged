#! /usr/bin/runhaskell

-- find the longest superstring of a set of strings
-- http://rosalind.info/problems/long/

import System.Environment
import Data.List (tails, maximumBy)
import Data.Function (on)
import Fasta

main :: IO ()
main = do
        args <- getArgs
        fasta <- if (length args) >= 1 then readFastaFile (head args) else return []
        sequences <- return (map recordSequence fasta)
        ss <- return (superstring sequences)
        putStrLn ss

data Fixity = Prefix | Suffix
    deriving (Eq, Show)
data Circumfix = Circumfix Fixity String Int
    deriving (Eq, Show)

superstring :: [String] -> String
superstring (x:xs) = reduce x xs

reduce :: String -> [String] -> String
reduce str [] = str
reduce str xs = let c@(Circumfix _ string _) = longestFix str xs
                     in reduce (combine str c) (remove string xs)
    where
        remove :: String -> [String] -> [String]
        remove _ [] = []
        remove str (s:ss)
            | s == str    = ss
            | otherwise   = s:(remove str ss)

combine :: String -> Circumfix -> String
combine [] (Circumfix _ str _) = str
combine xs (Circumfix Prefix str n) = str ++ drop n xs
combine xs (Circumfix Suffix str n) = xs ++ drop n str

longestFix :: String -> [String] -> Circumfix
longestFix str xs = prefixes xs (Circumfix Prefix [] 0)
    where
        prefixes :: [String] -> Circumfix -> Circumfix
        prefixes [] circumfix = suffixes xs circumfix
        prefixes (y:ys) (Circumfix p string n) = prefix (length y) (tails y) (Circumfix p string n)
            where
            prefix :: Int -> [String] -> Circumfix -> Circumfix
            prefix _ [] c = prefixes ys c
            prefix m (z:zs) (Circumfix _ string n)
                | m < n             = prefixes ys (Circumfix Prefix string n)
                | isPrefix z str    = prefixes ys (Circumfix Prefix y m)
                | otherwise         = prefix (m-1) zs (Circumfix Prefix string n)
            isPrefix :: String -> String -> Bool
            isPrefix [] _ = True
            isPrefix _ [] = False
            isPrefix (f:fs) (s:ss)
                | f == s    = isPrefix fs ss
                | otherwise = False
        suffixes :: [String] -> Circumfix -> Circumfix
        suffixes [] circumfix = circumfix
        suffixes (y:ys) (Circumfix p string n) = suffix (length str) (tails str) (Circumfix p string n)
            where
            suffix :: Int -> [String] -> Circumfix -> Circumfix
            suffix _ [] c = c
            suffix m (z:zs) (Circumfix p string n)
                | m < n           = suffixes ys (Circumfix p string n)
                | isSuffix y z    = suffixes ys (Circumfix Suffix y m)
                | otherwise       = suffix (m-1) zs (Circumfix p string n)
            isSuffix :: String -> String -> Bool
            isSuffix [] _ = False
            isSuffix _ [] = True
            isSuffix (f:fs) (s:ss)
                | f == s    = isSuffix fs ss
                | otherwise = False
