#! /usr/bin/runhaskell

-- reading a fasta file and choosing the entry with the highest GC content
-- http://rosalind.info/problems/gc/

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        putStr ((\(x, y) -> x ++ "\n" ++ show y) (maxGCContent (fastaParse contents)))

gcContent :: Float -> Float -> String -> Float
gcContent gc total [] = 100*(gc/total)
gcContent gc total (x:xs)
    | elem x "GC"    = gcContent (gc + 1) (total + 1) xs
    | otherwise      = gcContent gc (total + 1) xs

fastaParse :: String -> ([String], [String])
fastaParse file = fastaParse' (lines file)
    where
        fastaParse' :: [String] -> ([String], [String])
        fastaParse' [] = ([], [])
        fastaParse' (x:xs)
            | head x == '>'     = let (headers, sequences) = fastaParse' xs in (x:headers, sequences)
            | otherwise         = let (headers, sequences) = fastaParse' rest in (headers, dnastring:sequences)
                where
                    (dnastring, rest) = fastaParse'' (x:xs)
        fastaParse'' :: [String] -> (String, [String])
        fastaParse'' [] = ([], [])
        fastaParse'' (x:xs)
            | head x == '>'    = ([], x:xs)
            | otherwise        = let (dnastring, rest) = fastaParse'' xs in (x ++ dnastring, rest)

maxGCContent :: ([String], [String]) -> (String, Float)
maxGCContent (headers, sequences) = maxGCContent' 0 [] (headers, sequences)
    where
        maxGCContent' :: Float -> String -> ([String], [String]) -> (String, Float)
        maxGCContent' max maxH ([], []) = (maxH, max)
        maxGCContent' max maxH (h:headers, s:sequences)
            | gcContent 0 0 s >= max    = maxGCContent' (gcContent 0 0 s) h (headers, sequences)
            | otherwise                 = maxGCContent' max maxH (headers, sequences)
