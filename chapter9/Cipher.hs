module Cipher where

import Data.Char

caesarCipher :: Int -> [Char] -> [Char]
caesarCipher _ [] = []
caesarCipher  s (x: xs) = (chr . (+) 96 $ mod (ord x - 96 + s) 26) : caesarCipher s xs


caesarCipher' :: Int -> [Char] -> [Char]
caesarCipher'  s xs =  map (\x -> chr ( ( mod (ord x - 96 + s) 26)  + 96 ) )  xs


caesarCipher'' :: Int -> [Char] -> [Char]
caesarCipher''  s xs =  map (\x -> chr . (+) 96 $ mod (ord x - 96 + s) 26)  xs