#! /usr/bin/runhaskell

-- calculating a growing population of rabbits that die after a few months
-- http://rosalind.info/problems/fibd/

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        let (x:y:xs) = map read (words contents)::[Integer] in print (run x y)

type Hutch = [(Integer, Integer)]

run :: Integer -> Integer -> Integer
run n m = total (time (n-1) (initialise m))

initialise :: Integer -> Hutch
initialise n = (map (\x -> (x, 0)) (reverse [0 .. (n - 2)])) ++ [(n-1, 1)]

age :: Hutch -> Hutch
age hutch = age' 0 hutch
    where
        age' 0 ((0, r):hutch) = age' r hutch
        age' n ((a, r):[]) = [(a - 1, r), (a, n)]
        age' n ((a, r):hutch) = ((a-1, r):(age' (n+r) hutch))

time :: Integer -> Hutch -> Hutch
time 0 hutch = hutch
time n hutch = time (n-1) (age hutch)

total :: Hutch -> Integer
total [] = 0
total ((a, r):hutch) = r + (total hutch)
