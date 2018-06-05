eftBool :: Bool -> Bool -> [Bool]
eftBool True False = [True,False]
eftBool False True = []
eftBool True True = [True]
eftBool False False = [False]

-- LT | EQ | GT
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ LT = []
eftOrd EQ GT = [EQ, GT]
eftOrd GT GT = [GT]
eftOrd GT _ = []

eftInt :: Int -> Int -> [Int]
eftInt x y
 | x == y = [x]
 | x > y = []
 | otherwise = x : eftInt (x+1) y

eftChar :: Char -> Char -> [Char]
eftChar x y
 | x == y = [x]
 | x > y = []
 | otherwise = x : eftChar (succ(x))  y