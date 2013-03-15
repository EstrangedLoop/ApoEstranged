#! /usr/bin/runhaskell

import Data.List
import System.Environment
import Fasta

main = do
        args <- getArgs
        fasta <- readFastaFile (head args)
        strings <- return $ map recordSequence fasta
        putStrLn (foldr longestCommonSubstring [] strings)


longestCommonSubstring :: String -> String -> String
longestCommonSubstring [] ys = ys
longestCommonSubstring xs [] = xs
longestCommonSubstring xs ys = longestCommonSubstring' [] xs (tails ys)
    where
        longestCommonSubstring' :: String -> String -> [String] -> String
        longestCommonSubstring' best _ [] = best
        longestCommonSubstring' best [] (t:ts) = longestCommonSubstring' best xs ts
        longestCommonSubstring' best (s:str) (t:ts)
            | length best  > length t    = best
            | m > length best            = longestCommonSubstring' longest str (t:ts)
            | otherwise                  = longestCommonSubstring' best str (t:ts)
            where
                (m, longest) = match 0 (s:str) t
                match :: Int -> String -> String -> (Int, String)
                match n [] _ = (n, [])
                match n _ [] = (n, [])
                match n (a:as) (b:bs)
                    | a == b    = (fst (match (n+1) as bs), a:(snd (match (n+1) as bs)))
                    | otherwise = (n, [])
