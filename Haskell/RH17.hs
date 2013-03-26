#! /usr/bin/runhaskell

import System.Environment
import Translate
import Fasta
import Control.Monad
import Data.List

main :: IO ()
main = do
        args <- getArgs
        sequence <- liftM recordSequence (liftM head (readFastaFile (head args)))
        mapM_ putStrLn (map concat (map (map show) $ orfs (stringToDNA sequence)))

orfs :: [Nucleotide] -> [[Codon]]
orfs dna = let revcomp = reverse (complementDNA dna) in nub (filter (\x -> not (START `elem` x)) (concat (map orfs' [dna, tail dna, tail (tail dna), revcomp, tail revcomp, tail (tail revcomp)])))
    where
        orfs' :: [Nucleotide] -> [[Codon]]
        orfs' [] = []
        orfs' (x:y:z:xs)
            | [x, y, z] == [A, T, G]    = (Met : (getOrf xs)) : (orfs' xs)
            | otherwise                 = orfs' xs
        orfs' _ = [] 
        getOrf :: DNA -> [Codon]
        getOrf [] = [START]
        getOrf (x:y:z:xs)
            | translateDNACodon [x, y, z] == STOP    = []
            | otherwise                              = (translateDNACodon [x, y, z]) : (getOrf xs)
