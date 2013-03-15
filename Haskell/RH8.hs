#! /usr/bin/runhaskell

-- translating RNA into a protein
-- http://rosalind.info/problems/prot/

import System.Environment
import Translate

main :: IO ()
main = do
        args <- getArgs
        content <- readFile (head args)
        putStrLn content
        print (printProteinString $ translateRNA $ stringToRNA content)
