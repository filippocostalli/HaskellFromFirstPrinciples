
mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

{-
1. First write an expression that will make tuples of the outputs
of mySqr and myCube.
CHAPTER 9. THIS THING AND SOME MORE STUFF 485
2. Now alter that expression so that it only uses the x and y
values that are less than 50.
3. Apply another function to that list comprehension to
determine how many tuples inhabit your output list.
-}
makeTuples :: [(Integer, Integer)]
makeTuples = [(x,y) | x <- mySqr, y <- myCube]

makeTuplesLessFifty :: [(Integer, Integer)]
makeTuplesLessFifty = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

tuplesCount :: Int
tuplesCount = length makeTuplesLessFifty

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]