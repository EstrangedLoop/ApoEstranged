#! /usr/bin/runhaskell

-- calculate the weight of a protein
-- http://rosalind.info/problems/prtm/

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        print (foldr1 (+) (map weight contents))


weight :: Char -> Double
weight 'A' = 71.03711
weight 'C' = 103.00919
weight 'D' = 115.02694
weight 'E' = 129.04259
weight 'F' = 147.06841
weight 'G' = 57.02146
weight 'H' = 137.05891
weight 'I' = 113.08406
weight 'K' = 128.09496
weight 'L' = 113.08406
weight 'M' = 131.04049
weight 'N' = 114.04293
weight 'P' = 97.05276
weight 'Q' = 128.05858
weight 'R' = 156.10111
weight 'S' = 87.03203
weight 'T' = 101.04768
weight 'V' = 99.06841
weight 'W' = 186.07931
weight 'Y' = 163.06333
weight _ = 0
