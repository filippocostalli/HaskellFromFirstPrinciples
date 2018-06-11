module WriteYourStandard where

import Data.Char

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
    if x == False
    then False
    else myAnd xs

-- direct recursion, using (&&)
myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd' xs



-- myOr returns True if any Bool in the list is True.

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
    if x == True
    then True
    else myOr xs


myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = x || myOr' xs

{-
myAny returns True if a -> Bool applied to any of the values in the list returns True.
Example for validating myAny:
Prelude> myAny even [1, 3, 5]
False
Prelude> myAny odd [1, 3, 5]
True
-}

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f(x) || myAny f xs

{-
After you write the recursive myElem, write another version that uses any. 

The built-in version of elem in GHC 7.10 and newer has a type that uses Foldable instead of the list type specifically. 
You can ignore that and write the concrete version that works only for list.

myElem :: Eq a => a -> [a] -> Bool
Prelude> myElem 1 [1..10]
True
Prelude> myElem 1 [2..10]
False
-}

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = ( e == x ) || myElem e xs


myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs =  any (x==) xs

{-
Implement myReverse.
myReverse :: [a] -> [a]
Prelude> myReverse "blah"
"halb"
Prelude> myReverse [1..5]
[5,4,3,2,1]

-}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

{-
squish flattens a list of lists into a list
squish :: [[a]] -> [a]
-}

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

{-
squishMap maps a function over a list and concatenates the
results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined
Prelude> squishMap (\x -> [1, x, 3]) [2]
[1,2,3]
Prelude> squishMap (\x -> "WO "++[x]++" HOO ") "123"
"WO 1 HOO WO 2 HOO WO 3 HOO "
-}

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

{-
squishAgain flattens a list of lists into a list. This time re-use the squishMap function.

squishAgain :: [[a]] -> [a]
squishAgain = undefined
-}

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\x -> x) xs

{-
myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last
value that the comparison returned GT for. 
If you import maximumBy from Data.List, you’ll see the type is:
Foldable t => (a -> a -> Ordering) -> t a -> a

rather than
(a -> a -> Ordering) -> [a] -> a

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined

Prelude> let xs = [1, 53, 9001, 10]
Prelude> myMaximumBy compare xs
9001
-}

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x: xs) = if ( f x (head xs) == LT) then myMaximumBy f xs else myMaximumBy f (x: drop 1 xs)

{-
myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value
that the comparison returned LT for.

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined

Prelude> let xs = [1, 53, 9001, 10]
Prelude> myMinimumBy compare xs
1
-}

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy f (x: xs) = if ( f x (head xs) == GT) then myMinimumBy f xs else myMinimumBy f (x: drop 1 xs)


{-

Using the myMinimumBy and myMaximumBy functions, write your own versions of maximum and minimum. 

myMaximum :: (Ord a) => [a] -> a
myMaximum = undefined

myMinimum :: (Ord a) => [a] -> a
myMinimum = undefined

-}

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs


myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs