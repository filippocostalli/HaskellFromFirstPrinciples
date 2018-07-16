module VigenereCipher where

import Data.Char

type Keyword = String
type PlainText = String
type CipherText = String

type KeywordChar = Char


shift :: Char -> Int
shift x =  (-) (ord . toUpper $ x) 65

cryptChar :: KeywordChar -> Char -> Char
cryptChar ' ' _ = ' '
cryptChar s a = chr ( ( mod (ord (toUpper a) - 64 + shift s) 26)  + 64 )

keyArray :: Keyword -> PlainText -> String
keyArray key plain = createKeyArray 0 key plain where
    createKeyArray _ _  [] = []
    createKeyArray i k (p: ps) = 
        if p == ' ' 
            then ' ' : createKeyArray i k ps  
            else key !! (mod i  (length k) ) : createKeyArray (i+1) k ps 


vigenereCipher :: Keyword -> PlainText -> CipherText
vigenereCipher  k p =  zipWith cryptChar p (keyArray k p)



