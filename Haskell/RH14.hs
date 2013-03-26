#! /usr/bin/runhaskell

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        [gens, n] <- return (map read (words contents)::[Integer])
        print (moreThanKFromN (2^gens) n)
        

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

exactlyKFromN :: Integer -> Integer -> Double
exactlyKFromN n k = (fromIntegral (choose n k)) * (0.25**(fromIntegral k)) * (0.75**(fromIntegral (n-k)))

moreThanKFromN :: Integer -> Integer -> Double
moreThanKFromN n k = sum (map (\x -> exactlyKFromN n x) [k .. n])
