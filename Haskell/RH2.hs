#! /usr/bin/runhaskell

-- transcribing RNA into DNA
-- http://rosalind.info/problems/rna/

import System.Environment

transcribe :: String -> String
transcribe [] = []
transcribe ('T':xs) = 'U':(transcribe xs)
transcribe (x:xs) = x:(transcribe xs)

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        putStrLn (transcribe contents)
