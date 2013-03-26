#! /usr/bin/runhaskell

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        print (rnaIndex contents)

rnaIndex :: String -> Int
rnaIndex xs = foldr (\x -> \y -> (x * y) `mod` 1000000) 3 (map aaIndex xs)

aaIndex :: Char -> Int
aaIndex 'A' = 4 -- Ala
aaIndex 'R' = 6 -- Arg
aaIndex 'N' = 2 -- Asn
aaIndex 'D' = 2 -- Asp
aaIndex 'C' = 2 -- Cys
aaIndex 'E' = 2 -- Glu
aaIndex 'Q' = 2 -- Gln
aaIndex 'G' = 4 -- Gly
aaIndex 'H' = 2 -- His
aaIndex 'I' = 3 -- Ile
aaIndex 'L' = 6 -- Leu
aaIndex 'K' = 2 -- Lys
aaIndex 'M' = 1 -- Met
aaIndex 'F' = 2 -- Phe
aaIndex 'P' = 4 -- Pro
aaIndex 'S' = 6 -- Ser
aaIndex 'T' = 4 -- Thr
aaIndex 'W' = 1 -- Trp
aaIndex 'Y' = 2 -- Tyr
aaIndex 'V' = 4 -- Val
aaIndex '*' = 3 -- STOP
aaIndex _ = 1
