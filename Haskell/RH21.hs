#! /usr/bin/runhaskell

-- remove introns and calculate a protein
-- http://rosalind.info/problems/splc/

import System.Environment
import Fasta
import Control.Monad
import Translate

main :: IO ()
main = do
        args <- getArgs
        sequences <- liftM (map recordSequence) (readFastaFile (head args))
        (dna, introns) <- return (head sequences, tail sequences)
        putStrLn (init (concat (map show (translateDNA (stringToDNA (removeIntrons dna introns))))))

removeIntrons :: String -> [String] -> String
removeIntrons [] _ = []
removeIntrons xs [] = xs
removeIntrons xs (y:ys) = removeIntrons (removeIntrons' xs y) ys
    where
        removeIntrons' :: String -> String -> String
        removeIntrons' [] _ = []
        removeIntrons' (x:xs) y
            | take (length y) (x:xs) == y    = removeIntrons' (drop (length y) (x:xs)) y
            | otherwise                      = x : (removeIntrons' xs y)
