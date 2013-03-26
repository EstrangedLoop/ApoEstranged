#! /usr/bin/runhaskell

-- calculate all permutations of the first n positive integers
-- http://rosalind.info/problems/orf/

import System.Environment
import Data.List

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        int <- return (read contents::Int)
        putStrLn (show (length (permutations [1 .. int])))
        mapM_ (putStrLn.showIntList) (permutations [1 .. int])

showIntList :: [Int] -> String
showIntList [] = []
showIntList [x] = show x
showIntList (x:xs) = show x ++ " " ++ showIntList xs
