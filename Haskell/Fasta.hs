#! /usr/bin/runhaskell

-- a module for reading a manipulating FASTA files

module Fasta where

import Control.Monad

data FastaRecord = FastaRecord { header :: String, recordSequence :: String }
    deriving (Eq)

instance Show FastaRecord where
    show record = header record ++ "\n" ++ recordSequence record

type Fasta = [FastaRecord]

readFasta :: String -> Fasta
readFasta contents = readFasta' (lines contents)
    where
        readFasta' :: [String] -> Fasta
        readFasta' [] = []
        readFasta' (x:xs)
            | head x == '>'    = let (dnasequence, rest) = readFasta'' xs in (FastaRecord x dnasequence):(readFasta' rest)
            | otherwise        = readFasta' xs
            where
                readFasta'' :: [String] -> (String, [String])
                readFasta'' [] = ([], [])
                readFasta'' (s:sequences)
                    | head s == '>'    = ([], (s:sequences))
                    | otherwise        = let (dnasequence, rest) = readFasta'' sequences in (s ++ dnasequence, rest)

readFastaFile :: String -> IO [FastaRecord]
readFastaFile file = do
                 contents <- readFile file
                 return (readFasta contents)

printFasta :: Fasta -> String
printFasta fasta = unlines (map show fasta)
