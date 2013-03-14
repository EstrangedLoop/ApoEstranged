#! /usr/bin/runhaskell

-- calculating the points at which a motif matches a strin
-- http://rosalind.info/problems/subs/

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        content <- readFile (head args)
        let (x:y:xs) = lines content in putStrLn (printMotifPositions y x)

printMotifPositions :: String -> String -> String
printMotifPositions xs ys = printInts (motifPositions xs ys)
    where
    printInts :: [Int] -> String
    printInts [] = []
    printInts (x:xs) = show x ++ " " ++ (printInts xs) 

motifPositions :: String -> String -> [Int]
motifPositions xs ys = motifPositions' 1 xs ys
    where
    motifPositions' :: Int -> String -> String -> [Int]
    motifPositions' _ [] _ = []
    motifPositions' _ _ [] = []
    motifPositions' n (x:xs) (y:ys)
        | matchAt xs ys    = n:(motifPositions' (n + 1) (x:xs) ys)
        | otherwise          = motifPositions' (n + 1) (x:xs) ys
    matchAt :: String -> String -> Bool
    matchAt [] _ = True
    matchAt _ [] = False
    matchAt (x:xs) (y:ys)
        | x /= y    = False
        | otherwise = matchAt xs ys
