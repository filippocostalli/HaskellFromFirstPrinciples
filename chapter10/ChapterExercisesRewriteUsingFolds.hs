module ChapterExercisesRewriteUsingFolds where

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
    if x == False
        then False
        else myAnd xs

-- direct recursion, using (&&)
myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd xs

-- fold, not point-free in the folding function
myAnd'' :: [Bool] -> Bool
myAnd'' = foldr
    (\a b ->
    if a == False
    then False
    else b) True

-- fold, both myAnd and the folding  function are point-free now
myAnd''' :: [Bool] -> Bool
myAnd''' = foldr (&&) True

{-
1. myOr returns True if any Bool in the list is True.
    myOr :: [Bool] -> Bool
    myOr = undefined
-}
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myOr' :: [Bool] -> Bool
myOr' = foldr (\a b -> if a == True then True else b) False

{-
2. myAny returns True if a -> Bool applied to any of the values in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined
Example for validating myAny:
Prelude> myAny even [1, 3, 5]
False
Prelude> myAny odd [1, 3, 5]
True
-}
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\a b -> if f a == True then True else b) False xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr ((||) . f) False

{-
3. Write two versions of myElem. One version should use folding and the other should use any.
myElem :: Eq a => a -> [a] -> Bool
Prelude> myElem 1 [1..10]
True
Prelude> myElem 1 [2..10]
False
-}
myElemFoldr :: Eq a => a -> [a] -> Bool
myElemFoldr c = foldr ((||).(== c)) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny c = myAny' (==c)

{-
4. Implement myReverse, don’t worry about trying to make it lazy.
myReverse :: [a] -> [a]
myReverse = undefined
Prelude> myReverse "blah"
"halb"
Prelude> myReverse [1..5]
[5,4,3,2,1]
-}
myReverse :: [a] -> [a]
myReverse =  foldl (flip (:)) []

{-
5. Write myMap in terms of foldr. It should have the same behavior as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap = undefined
-}
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) [] 

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr (\a b -> f a : b) [] 

{-
6. Write myFilter in terms of foldr. It should have the same behavior as the built-in filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined
-}
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

{-
7. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = undefined
-}
squish :: [[a]] -> [a]
squish = foldr (++) []

{-
8. squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined
Prelude> squishMap (\x -> [1, x, 3]) [2]
[1,2,3]
Prelude> let f x = "WO " ++ [x] ++ " OT "
Prelude> squishMap f "blah"
"WO b OT WO l OT WO a OT WO h OT "
-}
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ( (++) . f) []

{-
9. squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = undefined
-}
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

{-
10. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last
value that the comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined
Prelude> myMaximumBy (\_ _ -> GT) [1..10]
1
Prelude> myMaximumBy (\_ _ -> LT) [1..10]
10
Prelude> myMaximumBy compare [1..10]
10
-}
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if f a b == GT then a else b) (last xs) xs

-- using foldl
myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f xs = foldl (\a b -> if f a b == GT then a else b) (head xs) xs

-- yet another way to skin the cat
myMaximumBy'' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy'' f (x:xs) = foldr (\b a -> if f a b == GT then a else b) x xs

{-11. myMinimumBy takes a comparison function and a list and
returns the least element of the list based on the last value
that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined
Prelude> myMinimumBy (\_ _ -> GT) [1..10]
10
Prelude> myMinimumBy (\_ _ -> LT) [1..10]
1
Prelude> myMinimumBy compare [1..10]
1
-}
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (last xs) xs