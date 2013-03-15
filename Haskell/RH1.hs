#! /usr/bin/runhaskell
-- counting the number of each nucleotide in a DNA string
-- http://rosalind.info/problems/dna/

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        putStrLn (format (numberNucleotides contents))

numberNucleotides :: String -> (Int, Int, Int, Int)
numberNucleotides [] = (0, 0, 0, 0)
numberNucleotides (x:xs)
    | x == 'A'    = (1 + a, c, g, t)
    | x == 'C'    = (a, 1 + c, g, t)
    | x == 'G'    = (a, c, g + 1, t)
    | x == 'T'    = (a, c, g, t + 1)
    | otherwise   = (a, c, g, t)
    where
        (a, c, g, t) = numberNucleotides xs

format :: (Int, Int, Int, Int) -> String
format (a, c, g, t) = show a ++ " " ++ show c ++ " " ++ show g ++ " " ++ show t
