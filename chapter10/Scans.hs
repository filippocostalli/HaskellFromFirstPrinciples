module Scans where


{-
scanl (+) 1 [1..3]
-- unfolding the
-- definition of scanl
= [ 1, 1 + 1
, (1 + 1) + 2
, ((1 + 1) + 2) + 3
]

-}

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

{-
1. Modify your fibs function to only return the first 20 Fibonacci numbers.

2. Modify fibs to return the Fibonacci numbers that are less than 100.

3. Try to write the factorial function from Recursion as a scan. You’ll want scanl again, and your start value will be 1. 
Warning: this will also generate an infinite list, so you may want to pass it through a take function or similar.
-}

fibs' :: [Integer]
fibs' = take 20  (1 : scanl (+) 1 fibs')

fibs'' :: [Integer]
fibs'' = filter (\x -> x < 100)  fibs'

myFact :: Int -> Integer
myFact x = (scanl (*) 1 [1..]) !! x