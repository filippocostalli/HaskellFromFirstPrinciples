module AsPatterns where

import Data.List
import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf subSeq mainSeq = subSeq == reduce subSeq mainSeq
    where reduce a b = nub $ filter (\x -> elem x a) b
    -- this is the same
    -- where reduce a b = (nub . filter (\x -> elem x a)) b

-- using as-pattern
isSubseqOf' :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf' [] _ = True
isSubseqOf' _ [] = False
isSubseqOf' sub@(x:xs) (y:ys)
  | x == y    = isSubseqOf' xs ys
  | otherwise = isSubseqOf' sub ys


capitalizeWords :: String  -> [(String, String)]
capitalizeWords [] = []
capitalizeWords a = map capitalizeWord (words a)

capitalizeWord :: String  -> (String, String)
capitalizeWord w@(x:xs) = (w, toUpper x : xs)
