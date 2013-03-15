#! /usr/bin/runhaskell

-- calculating the hamming distance between two strings
-- http://rosalind.info/problems/hamm/

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        let (first:second:xs) = lines contents in putStrLn (show (hamming first second))

hamming :: String -> String -> Int
hamming [] _ = 0
hamming _ [] = 0
hamming (x:xs) (y:ys)
    | x /= y    = 1 + (hamming xs ys)
    | otherwise = hamming xs ys
