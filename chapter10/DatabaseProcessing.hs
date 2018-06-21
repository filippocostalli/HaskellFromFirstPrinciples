module Databaseprocessing where

import Data.Time

data DatabaseItem = 
    DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime 
                (fromGregorian 1911 5 1) 
                (secondsToDiffTime 34123) 
            )
    , DbNumber 9001
    , DbNumber 1288
    , DbString "Hello, world!"
    , DbDate (UTCTime
    (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
    ]

{-
1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = undefined
-}
isDbDate :: DatabaseItem -> Bool
isDbDate (DbNumber _) = False
isDbDate (DbString _) = False
isDbDate (DbDate _) = True

getUTCTime:: DatabaseItem -> UTCTime
getUTCTime (DbDate x) = x
    
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate x = map getUTCTime y
    where y = filter (isDbDate) x

{-
Write a function that filters for DbNumber values and returns a list of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = undefined
-}
isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber (DbString _) = False
isDbNumber (DbDate _) = False

getInteger:: DatabaseItem -> Integer
getInteger (DbNumber x) = x

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber x = map getInteger y
    where y = filter (isDbNumber) x

{-
3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = undefined
-}
-- NOTE: The book still has not introduced Maybe, so I don't use it
getMax :: Ord a => [a] -> a
getMax (x:[]) = x
getMax (x:xs) = if (x > head xs) then getMax (x : tail xs) else getMax xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = getMax $ filterDbDate xs

{-
4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = undefined
-}
sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 (filterDbNumber xs)

{-
5. Write a function that gets the average of the DbNumber values.
-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb = undefined
-}

avgDb :: [DatabaseItem] -> Double
avgDb [] = 0
avgDb xs = (fromIntegral $ sumDb xs) /  ( fromIntegral . length $ filterDbNumber xs)
