incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n 
incTimes x n = n + incTimes (x-1) n


applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)


incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' 0 n = n
incTimes' x n = applyTimes x (+1) n

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes (n-1) f $ b

{-
applyTimes 5 (+1) 5

( +1 (:q
))

-}

fibonacci :: Integral a => a -> a 
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


type Numerator = Integer
type Denumerator = Integer
type Quotient = Integer

-- dividedBy :: Numerator -> Denumerator -> Quotient 
-- dividedBy = div

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0  
    where 
        go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

