#! /usr/bin/runhaskell

-- calculating the chance of a dominant phenotype from a mixed population
-- http://rosalind.info/problems/iprb/

import System.Environment

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile (head args)
        let (x:y:z:xs) = map read (words contents) in putStrLn $ show $ domPhenoChance (Population x y z)

data Population = Population { homozygousDominant :: Int, heterozygous :: Int, homozygousRecessive :: Int }

total :: Population -> Int
total pop = homozygousDominant pop + heterozygous pop + homozygousRecessive pop

pairings :: [[Int]]
pairings = let three = [1, 2, 3] in [ [x, y] | x <- three, y <- three]

encodeType :: Int -> Population -> Int
encodeType 1 p = homozygousDominant p
encodeType 2 p = heterozygous p
encodeType 3 p = homozygousRecessive p

dominanceChance :: Int -> Float
dominanceChance 1 = 1.0
dominanceChance 2 = 0.5
dominanceChance 3 = 0.0

choicePopulation :: [Int] -> Population -> Float
choicePopulation (x:y:xs) p
    | x == y    = (fromIntegral (encodeType x p))/(fromIntegral (total p)) * (fromIntegral ((encodeType y p) - 1))/(fromIntegral ((total p) - 1))
    | otherwise    = (fromIntegral (encodeType x p))/(fromIntegral (total p)) * (fromIntegral (encodeType y p))/(fromIntegral ((total p) - 1))

choiceDom :: [Int] -> Float
choiceDom (x:y:xs) = 1 - ((1 - (dominanceChance x)) * (1 - (dominanceChance y))) 

domPhenoChance :: Population -> Float
domPhenoChance p = sum $ map (\x -> ((choiceDom x)*(choicePopulation x p))) pairings
