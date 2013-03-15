#! /usr/bin/runhaskell

-- calculated the expected prevalence of a dominant phenotype in the next generation
-- http://rosalind.info/problems/iev/

import System.Environment

main = do
        args <- getArgs
        contents <- readFile (head args)
        let xs = map read (words contents)::[Float] in print (2*(sum (zipWith (*) chances xs)))

chances :: [Float]
chances = [1.0, 1.0, 1.0, 0.75, 0.5, 0.0]
