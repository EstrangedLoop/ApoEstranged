#! /usr/bin/runhaskell

-- a module with functions and datatypes to do with DNA/RNA translation and transcription

module Translate where

import System.Environment

data Nucleotide = A | T | C | G | U
    deriving (Show, Eq)

type DNA = [Nucleotide]
type RNA = [Nucleotide]

stringToDNA :: String -> DNA
stringToDNA [] = []
stringToDNA (x:xs)
    | x == ' '    = stringToDNA xs
    | x == '\n'    = stringToDNA xs
    | x == 'A'    = A:(stringToDNA xs)
    | x == 'T'    = T:(stringToDNA xs)
    | x == 'C'    = C:(stringToDNA xs)
    | x == 'G'    = G:(stringToDNA xs)
    | otherwise   = error ("Unrecognised nucleotide: " ++ (x:[]) ++ "\n")

stringToRNA :: String -> RNA
stringToRNA [] = []
stringToRNA (x:xs)
    | x == ' '    = stringToDNA xs
    | x == '\n'    = stringToDNA xs
    | x == 'A'    = A:(stringToRNA xs)
    | x == 'U'    = U:(stringToRNA xs)
    | x == 'C'    = C:(stringToRNA xs)
    | x == 'G'    = G:(stringToRNA xs)
    | otherwise   = error ("Unrecognised nucleotide: " ++ x:[] ++ "\n")

transcribe :: DNA -> RNA
transcribe [] = []
transcribe (x:xs)
    | x == T    = U:(transcribe xs)
    | otherwise   = x:(transcribe xs)

complementDNA :: DNA -> DNA
complementDNA [] = []
complementDNA (T:xs) = A:(complementDNA xs)
complementDNA (A:xs) = T:(complementDNA xs)
complementDNA (G:xs) = C:(complementDNA xs)
complementDNA (C:xs) = G:(complementDNA xs)

complementRNA :: RNA -> RNA
complementRNA [] = []
complementRNA (U:xs) = A:(complementDNA xs)
complementRNA (A:xs) = U:(complementDNA xs)
complementRNA (G:xs) = C:(complementDNA xs)
complementRNA (C:xs) = G:(complementDNA xs)

data Codon = Ala | Arg | Asn | Asp | Cys | Glu | Gln | Gly
                 | His | Ile | Leu | Lys | Met | Phe | Pro | Ser
                 | Thr | Trp | Tyr | Val | STOP | START
    deriving (Eq)

instance Show Codon where
    show Ala = "A"
    show Arg = "R"
    show Asn = "N"
    show Asp = "D"
    show Cys = "C"
    show Glu = "E"
    show Gln = "Q"
    show Gly = "G"
    show His = "H"
    show Ile = "I"
    show Leu = "L"
    show Lys = "K"
    show Met = "M"
    show Phe = "F"
    show Pro = "P"
    show Ser = "S"
    show Thr = "T"
    show Trp = "W"
    show Tyr = "Y"
    show Val = "V"
    show STOP = "*"

translateRNACodon :: RNA -> Codon
translateRNACodon [] = undefined
translateRNACodon [x] = undefined
translateRNACodon [x, y] = undefined
translateRNACodon (x:y:z:xs)
    | (x, y, z) == (U, U, U)    = Phe
    | (x, y, z) == (U, U, C)    = Phe

    | (x, y, z) == (U, U, A)    = Leu
    | (x, y, z) == (U, U, G)    = Leu
    | (x, y, z) == (C, U, U)    = Leu
    | (x, y, z) == (C, U, C)    = Leu
    | (x, y, z) == (C, U, A)    = Leu
    | (x, y, z) == (C, U, G)    = Leu

    | (x, y, z) == (A, U, U)    = Ile
    | (x, y, z) == (A, U, C)    = Ile
    | (x, y, z) == (A, U, A)    = Ile

    | (x, y, z) == (A, U, G)    = Met

    | (x, y, z) == (G, U, U)    = Val
    | (x, y, z) == (G, U, C)    = Val
    | (x, y, z) == (G, U, A)    = Val
    | (x, y, z) == (G, U, G)    = Val

    | (x, y, z) == (U, C, U)    = Ser
    | (x, y, z) == (U, C, C)    = Ser
    | (x, y, z) == (U, C, A)    = Ser
    | (x, y, z) == (U, C, G)    = Ser

    | (x, y, z) == (C, C, U)    = Pro
    | (x, y, z) == (C, C, C)    = Pro
    | (x, y, z) == (C, C, A)    = Pro
    | (x, y, z) == (C, C, G)    = Pro

    | (x, y, z) == (A, C, U)    = Thr
    | (x, y, z) == (A, C, C)    = Thr
    | (x, y, z) == (A, C, A)    = Thr
    | (x, y, z) == (A, C, G)    = Thr

    | (x, y, z) == (G, C, U)    = Ala
    | (x, y, z) == (G, C, C)    = Ala
    | (x, y, z) == (G, C, A)    = Ala
    | (x, y, z) == (G, C, G)    = Ala

    | (x, y, z) == (U, A, U)    = Tyr
    | (x, y, z) == (U, A, C)    = Tyr

    | (x, y, z) == (U, A, A)    = STOP
    | (x, y, z) == (U, A, G)    = STOP

    | (x, y, z) == (C, A, U)    = His
    | (x, y, z) == (C, A, C)    = His

    | (x, y, z) == (C, A, A)    = Gln
    | (x, y, z) == (C, A, G)    = Gln

    | (x, y, z) == (A, A, U)    = Asn
    | (x, y, z) == (A, A, C)    = Asn

    | (x, y, z) == (A, A, A)    = Lys
    | (x, y, z) == (A, A, G)    = Lys

    | (x, y, z) == (G, A, U)    = Asp
    | (x, y, z) == (G, A, C)    = Asp

    | (x, y, z) == (G, A, A)    = Glu
    | (x, y, z) == (G, A, G)    = Glu

    | (x, y, z) == (U, G, U)    = Cys
    | (x, y, z) == (U, G, C)    = Cys

    | (x, y, z) == (U, G, A)    = STOP

    | (x, y, z) == (U, G, G)    = Trp

    | (x, y, z) == (C, G, U)    = Arg
    | (x, y, z) == (C, G, C)    = Arg
    | (x, y, z) == (C, G, A)    = Arg
    | (x, y, z) == (C, G, G)    = Arg

    | (x, y, z) == (A, G, U)    = Ser
    | (x, y, z) == (A, G, C)    = Ser

    | (x, y, z) == (A, G, A)    = Arg
    | (x, y, z) == (A, G, G)    = Arg

    | (x, y, z) == (G, G, U)    = Gly
    | (x, y, z) == (G, G, C)    = Gly
    | (x, y, z) == (G, G, A)    = Gly
    | (x, y, z) == (G, G, G)    = Gly
    | otherwise                 = error (show (x, y, z))

translateDNACodon :: DNA -> Codon
translateDNACodon = translateRNACodon.transcribe

translateDNA :: DNA -> [Codon]
translateDNA [] = []
translateDNA [x] = []
translateDNA [x, y] = []
translateDNA (x:y:z:xs) = (translateDNACodon [x, y, z]):(translateDNA xs)

translateRNA :: RNA -> [Codon]
translateRNA [] = []
translateRNA [x] = []
translateRNA [x, y] = []
translateRNA (x:y:z:xs) = (translateRNACodon [x, y, z]):(translateRNA xs)

printProteinString :: [Codon] -> String
printProteinString [] = []
printProteinString (x:xs) = show x ++ (printProteinString xs)
