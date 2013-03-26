#! /usr/bin/runhaskell

import System.Environment
import Control.Monad

main :: IO ()
main  = do
         args <- getArgs
         [letters, len] <- liftM lines (readFile (head args))
         alphabet <- return (map head (words letters))
         mapM_ putStrLn (nWords (read len) alphabet)

nWords :: Int -> String -> [String]
nWords n aleph = nWords' n aleph
    where
        nWords' :: Int -> String -> [String]
        nWords' 0 _ = []
        nWords' _ [] = []
        nWords' 1 alphabet = map (\x -> [x]) alphabet
        nWords' n (a:alphabet) = map (a:) (nWords' (n-1) aleph) ++ nWords' n alphabet
