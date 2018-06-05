module WordNumber where

-- import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digitToWordGuards :: Int -> String
digitToWordGuards n 
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | otherwise = "zero"

digitToWordCase :: Int -> String 
digitToWordCase n = 
    case n of
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"
        otherwise -> "zero"



digits :: Int -> [Int]
digits n = reverse inv
    where 
        inv = myDigits n
        myDigits x 
            | x < 10 = x:[]
            | otherwise = (mod x 10) : (myDigits (div x 10))

{-
myDigits :: Int -> [Int]
myDigits x 
    | x < 10 = x:[]
    | otherwise = (mod x 10) : (myDigits (div x 10))
-}


wordNumber :: Int -> String
-- wordNumber n = unwords (map digitToWordCase (digits n))
-- wordNumber n = unwords $ map digitToWordCase (digits n)
wordNumber n = (unwords . map digitToWordCase . digits) n

-- Noter: I used unwords, another acceptable solution could use interspecte. It depends on the format you want the function to return