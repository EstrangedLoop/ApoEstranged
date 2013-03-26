#! /usr/bin/runhaskell

import System.Environment
import Network.HTTP.Wget
import Control.Monad
import Fasta

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        urls <- return (let prots = lines contents in map (\x -> "http://www.uniprot.org/uniprot/" ++ x ++ ".fasta") prots)
        records <- mapM (\z -> wget z [] []) urls
        fasta <- return (readFasta (concat records))
        positions <- return (filter (/= []) (zipWith (\x -> \y -> if y == [] then "" else x ++ "\n" ++ showIntList y) (lines contents)  (map (\x -> nGlyPositions (recordSequence x)) fasta)))
        mapM_ putStrLn positions


nglycosylation :: String -> Bool
nglycosylation [] = False
nglycosylation (x:y:z:w:str) = x == 'N' && y /= 'P' && (z == 'S' || z == 'T') && w /= 'P'
nglycosylation _ = False

nGlyPositions :: String -> [Int]
nGlyPositions str = nGlyPositions' str 1
    where
        nGlyPositions' [] _ = []
        nGlyPositions' (x:xs) n
            | nglycosylation (x:xs)    = n : (nGlyPositions' xs (n+1))
            | otherwise                = nGlyPositions' xs (n+1)

showIntList :: [Int] -> String
showIntList [x] = show x
showIntList (x:xs) = show x ++ " " ++ showIntList xs
showIntList [] = []
