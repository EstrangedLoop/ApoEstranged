#! /usr/bin/runhaskell

-- calculating the number of rabbits after n months
-- http://rosalind.info/problems/fib/

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        print (let (a, b) = parseFile contents in expansion a b)

expansion :: Integer -> Integer -> Integer
expansion 1 k = 1
expansion 2 k = 1
expansion n k = ((expansion (n - 1) k) + k*(expansion (n - 2) k))

parseFile :: String -> (Integer, Integer)
parseFile str = (a, b)
    where
        (a, b) = (head nums, head (tail nums))
        nums = (map read (words str))::[Integer]
