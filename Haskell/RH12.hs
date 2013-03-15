#! /usr/bin/runhaskell

-- build a graph based on a suffix-prefix relation
-- http://rosalind.info/problems/grph/

import System.Environment
import Fasta
import Control.Monad

main :: IO ()
main = do
        args <- getArgs
        fasta <- readFastaFile (head args);
        putStrLn (graph fasta)

prefixSuffix :: String -> String -> Bool
prefixSuffix (x:y:z:xs) (ts) = prefixSuffix' [x, y, z] ts
    where
    prefixSuffix' :: String -> String -> Bool
    prefixSuffix' _ [] = False
    prefixSuffix' start (x:y:z:[]) = start == [x, y, z] 
    prefixSuffix' start (x:xs) = prefixSuffix' start xs

graph :: Fasta -> String
graph fasta = graph' mix
    where
        mix :: [(FastaRecord, FastaRecord)]
        mix = [ (x,y) | x <- fasta, y <- fasta, x /= y]
        graph' :: [(FastaRecord, FastaRecord)] -> String
        graph' [] = []
        graph' (x:xs)
            | prefixSuffix (recordSequence (fst x)) (recordSequence (snd x))    = (tail (header (snd x))) ++ " " ++ (tail (header (fst x))) ++ "\n" ++ graph' xs
            | otherwise                                                         = graph' xs
       
         
