#! /usr/bin/runhaskell

-- generating a profile and making a consensus strings from several DNA strings
-- http://rosalind.info/problems/cons/

import System.Environment
import Control.Monad
import Fasta

main :: IO ()
main = do
        args <- getArgs
        fasta <- readFastaFile (head args)
        putStrLn (printProfile (profile fasta))

data Count = Count { a :: Int, c :: Int, g :: Int, t :: Int }
    deriving (Show, Eq)

type Profile = [Count]

sequences :: Fasta -> [String]
sequences = map recordSequence 

countUp :: String -> Count
countUp xs = countUp' 0 0 0 0 xs
    where
        countUp' :: Int -> Int -> Int -> Int -> String -> Count
        countUp' x y z w [] = (Count x y z w)
        countUp' x y z w (r:rs)
            | r == 'A'    = countUp' (x+1) y z w rs
            | r == 'C'    = countUp' x (y+1) z w rs
            | r == 'G'    = countUp' x y (z+1) w rs
            | r == 'T'    = countUp' x y z (w+1) rs
            | otherwise   = countUp' x y z w rs

profile :: Fasta -> Profile
profile [] = []
profile xs = profile' (sequences xs)
    where
        profile' :: [String] -> Profile
        profile' ys
            | head ys == []    = []
            | otherwise        = (countUp heads):(profile' tails)
            where
                (heads, tails) = (map head ys, map tail ys)
        

printProfile :: Profile -> String
printProfile p = consensus' p ++ aString ++ cString ++ gString ++ tString
    where
        aString :: String
        aString = "\nA:" ++ foldr1 (++) (map (\x -> " " ++ show (a x)) p)
        cString :: String
        cString = "\nC:" ++ foldr1 (++) (map (\x -> " " ++ show (c x)) p)
        gString :: String
        gString = "\nG:" ++ foldr1 (++) (map (\x -> " " ++ show (g x)) p)
        tString :: String
        tString = "\nT:" ++ foldr1 (++) (map (\x -> " " ++ show (t x)) p)


maxCount :: Count -> Char
maxCount (Count x y z w)
    | x == m    = 'A'
    | y == m    = 'C'
    | z == m    = 'G'
    | w == m    = 'T'
    where
        m :: Int
        m = max x (max y (max z w))

consensus' :: Profile -> String
consensus' = map maxCount

consensus :: Fasta -> String
consensus fasta = consensus' (profile fasta)
