#! /usr/bin/runhaskell

-- locate reverse palindrome positions
-- http://rosalind.info/problems/revp/

import System.Environment
import Translate
import Fasta

main :: IO ()
main = do
        args <- getArgs
        fasta <- readFastaFile (head args)
        sequence <- return (recordSequence (head fasta))
        mapM_ putStrLn (map (\x -> show (fst x) ++ " " ++ show (snd x)) (reversePalindromes sequence 1))

reversePalindromes :: String -> Int -> [(Int, Int)]
reversePalindromes [] _ = []
reversePalindromes (s:str) m = reversePalindromes' 4 ++ reversePalindromes str (m + 1)
    where
        reversePalindromes' :: Int -> [(Int, Int)]
        reversePalindromes' 13 = []
        reversePalindromes' n
            | length (s:str) < n                                                                       = []
            | let sub = take n (s:str) in stringToDNA sub == reverse (complementDNA (stringToDNA sub)) = (m, n) : (reversePalindromes' (n + 1))
            | otherwise                                                                                = reversePalindromes' (n + 1)
