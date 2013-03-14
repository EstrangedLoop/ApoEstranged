#! /usr/bin/runhaskell

-- reverse complementing a DNA strand
-- http://rosalind.info/problems/revc/

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        putStrLn (reverseComplement contents)

reverseComplement :: String -> String
reverseComplement [] = []
reverseComplement (x:xs)
    | x == 'A'    = (reverseComplement xs) ++ "T"
    | x == 'T'    = (reverseComplement xs) ++ "A"
    | x == 'C'    = (reverseComplement xs) ++ "G"
    | x == 'G'    = (reverseComplement xs) ++ "C"
    | otherwise   = reverseComplement xs
