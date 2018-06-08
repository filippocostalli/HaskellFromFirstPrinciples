module ChapterExercises where

import Data.Char

filterUpperCase :: [Char] -> [Char]
filterUpperCase xs = filter (\x -> isUpper x) xs


capitalizeFirst :: [Char] -> [Char]
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs

capitalizeFirst' :: [Char] -> [Char]
capitalizeFirst' [] = []
capitalizeFirst' xs = (toUpper $ (!!) xs 0)  :  drop 1 xs 


capitalizeAll :: [Char] -> [Char]
capitalizeAll [] = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

capitalizeAndReturnFirst :: [Char] -> Maybe Char
capitalizeAndReturnFirst [] = Nothing
capitalizeAndReturnFirst (x:_) = Just (toUpper x)

capitalizeAndReturnHead :: [Char] -> Maybe Char
capitalizeAndReturnHead [] = Nothing
capitalizeAndReturnHead x =  Just ( (toUpper . head) x )

capitalizeAndReturnHead' :: [Char] -> Maybe Char
capitalizeAndReturnHead' [] = Nothing
capitalizeAndReturnHead' x =  Just ( toUpper $ head x )