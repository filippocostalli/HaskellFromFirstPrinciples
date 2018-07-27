module Chapter12 where 

import Data.List

{-1. Write a recursive function named replaceThe which takes a
text/string, breaks it into words and replaces each instance
of “the” with “a”. It’s intended only to replace exactly
the word “the”. notThe is a suggested helper function for
accomplishing this.-}
-- example GHCi session
-- above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe x 
    | x == "the" = Nothing 
    | otherwise = Just x 


replaceThe' :: [String] -> [String]
replaceThe' [] = []
replaceThe' (x:xs) = case notThe x of
    Just a -> [a] ++ replaceThe' xs
    Nothing -> ["a"] ++ replaceThe' xs

-- Solution for which we're forced to use notThe and Maybe. Awful.
replaceThe :: String -> String
replaceThe = unwords . replaceThe' . words

-- Other way of using notThe
replaceThe'' :: String -> String
replaceThe'' = unwords . map (\x -> if notThe x == Nothing then "a" else x) . words

-- without using helper function. A lot simpler and clearer imho.
replaceTheBetter :: String -> String
replaceTheBetter = unwords . map (\x -> if x == "the" then "a" else x) . words 

{-2. Write a recursive function that takes a text/string, breaks
it into words, and counts the number of instances of ”the”
followed by a vowel-initial word.-}
-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
counter :: String -> String -> Integer
counter a b 
    | a == "the" && elem (head b) "aeiou" = 1
    | otherwise = 0

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = myCount . words
    where 
        myCount [] = 0
        myCount (_:[]) = 0
        myCount (x1:x2:xs) = (counter x1 x2) + myCount(x2:xs)

{-
3. Return the number of letters that are vowels in a word.
Hint: it’s helpful to break this into steps. Add any helper functions necessary to achieve your objectives.
a) Test for vowelhood
b) Return the vowels of a string
c) Count the number of elements returned
-}
-- helper function
isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

-- first solution: using a recursive function
countVowels :: String -> Integer
countVowels [] = 0
countVowels (x:xs) = case isVowel x of
    True -> 1 + countVowels(xs)
    False -> countVowels(xs)

-- another way: filter and count. much much better! (fromIntegral is used in order to return an Integer, as we are asked to)
countVowels' :: String -> Integer
countVowels' = fromIntegral . length . filter isVowel

{- VALIDATE THE WORDS
Use the Maybe type to write a function that counts the number of vowels in a string and the number of consonants. 
If the number of vowels exceeds the number of consonants, the function returns Nothing. 
In many human languages, vowels rarely exceed the number of consonants so when they do, it may indicate the input isn’t a word (that is, a valid input to your
dataset):
-}

isNotVowel :: Char -> Bool
isNotVowel x = notElem x "aeiou"

countNotVowels :: String -> Integer
countNotVowels = fromIntegral . length . filter isNotVowel

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = case (countNotVowels s - countVowels s < 0 ) of
    True -> Nothing
    False -> Just (Word' s)

{-
It’s only Natural
You’ll be presented with a datatype to represent the natural numbers. 
The only values representable with the naturals are whole numbers from zero to infinity. 
Your task will be to implement functions to convert Naturals to Integers and Integers to Naturals. 
The conversion from Naturals to Integers won’t return Maybe because Integer is a strict superset of Natural.
Any Natural can be represented by an Integer, but the same is not true of any Integer. Negative numbers are not valid natural numbers.
-}

-- As natural as any competitive bodybuilder
data Nat = Zero | Succ Nat deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing

intToNatRaw :: Integer -> Nat
intToNatRaw 0 = Zero
intToNatRaw n = Succ (intToNatRaw (n-1))

integerToNat :: Integer -> Maybe Nat
integerToNat x = case compare x 0 of
    LT -> Nothing
    _ -> Just (intToNatRaw x)
     
{-
Small library for Maybe
Write the following functions. This may take some time.
1. Simple boolean checks for Maybe values.
-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
-}
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

{-
2. The following is the Maybe catamorphism. You can turn a
Maybe value into anything else with this.
-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
-}
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee c _ Nothing = c 
mayybee _ f (Just x) = f x  

{-3. In case you just want to provide a fallback value.
-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
-- Try writing it in terms
-- of the maybe catamorphism
-}
fromMaybe :: a -> Maybe a -> a
fromMaybe c Nothing = c
fromMaybe _ (Just x) = x

{-4. Converting between List and Maybe.
-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
-}
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing 
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

{-
5. For when we want to drop the Nothing values from our list.
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> let xs = take 3 $ repeat Nothing
-- >>> catMaybes xs
-- []
catMaybes :: [Maybe a] -> [a]
-}
catMaybes :: [Maybe a] -> [a]
catMaybes = map (\(Just x) -> x) . filter (\x -> isJust x) 

{-6. You’ll see this called “sequence” later.
-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
-}
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs
    | any isNothing xs = Nothing
    | otherwise        = Just (catMaybes xs)

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' xs = case any isNothing xs of
    True  -> Nothing
    False -> Just (catMaybes xs)



{-
Small library for Either
Write each of the following functions. If more than one possible unique function exists for the type, use common sense to determine what it should do.
1. Try to eventually arrive at a solution that uses foldr, even if earlier versions don’t use foldr.
lefts' :: [Either a b] -> [a]
-}
lefts' :: [Either a b] -> [a]
lefts' = foldr pickupLeft []
  where
    pickupLeft (Left a) xs  = a : xs
    pickupLeft (Right _) xs = xs

-- another way
isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

lefts'' :: [Either a b] -> [a]
lefts'' =  map (\(Left a) -> a)  . filter isLeft

{- 2. Same as the last one. Use foldr eventually.
rights' :: [Either a b] -> [b]
-}
rights' :: [Either a b] -> [b]
rights' = foldr pickupRight []
  where
    pickupRight (Left _) xs  = xs
    pickupRight (Right b) xs = b:xs

{- 3. partitionEithers' :: [Either a b] -> ([a], [b])-}
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

{- 4. eitherMaybe' :: (b -> c)
    -> Either a b
    -> Maybe c
-}
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e = case e of
    Left _ -> Nothing
    Right a -> Just (f a) 

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right a) = Just (f a)


{-5. This is a general catamorphism for Either values.
either' :: (a -> c)
-> (b -> c)
-> Either a b
-> c
-}
either' :: (a -> c) -> (b -> c) -> Either a b-> c
either' f _ (Left a)  = f a
either' _ f (Right b) = f b

{-6. Same as before, but use the either' function you just wrote.
eitherMaybe'' :: (b -> c)
-> Either a b
-> Maybe c
-}
eitherMaybe''' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe''' f e = either' (\_ -> Nothing) (Just . f) e

------------------------- unfold
mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
    where 
        go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n+x) xs)


niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
    where 
        go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n*x) xs)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1


mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
    where 
    go :: [a] -> [[a]] -> [a]
    go xs' [] = xs'
    go xs' (x:xs) = (go (xs' ++ x) xs)

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []



{-
Write your own iterate and unfoldr
1. Write the function myIterate using direct recursion. 
Compare the behavior with the built-in iterate to gauge correctness.
Do not look at the source or any examples of iterate so that you are forced to do this yourself.
myIterate :: (a -> a) -> a -> [a]
-}
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

{-2. Write the function myUnfoldr using direct recursion. 
Compare with the built-in unfoldr to check your implementation.
Again, don’t look at implementations of unfoldr so that you figure it out yourself.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-}
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing -> []
    Just(a, b) -> a :  myUnfoldr f b


{-3. Rewrite myIterate into betterIterate using myUnfoldr. 
A hint — we used unfoldr to produce the same results as iterate earlier. 
Do this with different functions and see if you can abstract the structure out.
It helps to have the types in front of you
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr ...?
Remember, your betterIterate should have the same results as iterate.
-}
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\n -> Just(n , f n)) x
