{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Goats where

{-
1. Reusing the TooMany typeclass, write an instance of the typeclass for the type (Int, String). 
This will require adding a language pragma named FlexibleInstances if you do not use a newtype 
GHC will tell you what to do.
-}
class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany (n) = n > 43

instance TooMany (Int, String) where
    tooMany (n,s) = n > 43 && s == "goat"


{-
2. Make another TooMany instance for (Int, Int). 
Sum the values together under the assumption this is a count of goats from two fields.
-}
instance TooMany (Int, Int) where
    tooMany (n1, n2) = n1 + n2 > 86

{-
3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a).
This can mean whatever you want, such as summing the two numbers together.
-}
instance (Num a, TooMany a) => TooMany(a,a) where
    tooMany (n1, n2) = tooMany n1 && tooMany n2



newtype Goats = Goats Int  deriving (Eq, Show, TooMany)

