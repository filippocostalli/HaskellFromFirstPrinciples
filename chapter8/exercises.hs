-- chapter exercises
{-
Review of types
1. What is the type of [[True, False], [True, True], [False,
True]]?
a) Bool
b) mostly True
c) [a]
d) [[Bool]]

d
---------------------------------------
2. Which of the following has the same type as [[True, False], [True, True], [False, True]]?

a) [(True, False), (True, True), (False, True)]
b) [[3 == 3], [6 > 5], [3 < 4]]
c) [3 == 3, 6 > 5, 3 < 4]
d) ["Bool", "more Bool", "Booly Bool!"]

b
----------------------------------------

3. For the following function
func :: [a] -> [a] -> [a]
func x y = x ++ y
which of the following is true?
a) x and y must be of the same type
b) x and y must both be lists
c) if x is a String then y must be a String
d) all of the above

a-c

4. For the func code above, which is a valid application of
func to both of its arguments?
a) func "Hello World"
b) func "Hello" "World"
c) func [1, 2, 3] "a, b, c"
d) func ["Hello", "World"]

b
-}

-- REVIEWING CURRY
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
{-
1. What is the value of appedCatty "woohoo!" ? Try to determine the answer for yourself, then test in the REPL.
mine: "woops mrow woohoo!""
repl: "woops mrow woohoo!"

2. frappe "1" 
= flippy "haha" "1" = flip cattyConny "haha" "1" = "1 mrow haha"
mine: "1 mrow haha"
repl: "1 mrow haha"

3. frappe (appedCatty "2")
= frappe (cattyConny "woops" "2") = frappe "woops mrow 2" = flippy "haha" "woops mrow 2" = flip cattyConny "haha" "woops mrow 2" = "woops mrow 2 mrow haha"
mine: "woops mrow 2 mrow haha"
repl: "woops mrow 2 mrow haha"

4. appedCatty (frappe "blue")
= appedCatty (flippy "haha" "blue") = appedCatty (flip cattyConny "haha" "blue") = appedCatty "blue mrow haha" = cattyConny "woops" "blue mrow haha" = "woops mrow blue mrow haha"
mine: "woops mrow blue mrow haha"
repl: "woops mrow blue mrow haha"

5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
= cattyConny "pink mrow haha" (cattyConny "green" "woops mrow blue") = cattyConny "pink mrow haha" "green mrow woops mrow blue" = "pink mrow haha mrow green mrow woops mrow blue"
mine: "pink mrow haha mrow green mrow woops mrow blue"
repl: "pink mrow haha mrow green mrow woops mrow blue"

6. cattyConny (flippy "Pugs" "are") "awesome"
= cattyConny "are mrow Pugs" "awesome" = "are mrow Pugs mrow awesome"
mine: "are mrow Pugs mrow awesome"
repl: "are mrow Pugs mrow awesome"
-}

-- RECURSION
{-
Recursion
1. Write out the steps for reducing dividedBy 15 2 to its final
answer according to the Haskell code.

2. Write a function that recursively sums all numbers from
1 to n, n being the argument. So that if n was 5, you’d add
1 + 2 + 3 + 4 + 5 to get 15. The type should be (Eq a, Num a)
=> a -> a.

3. Write a function that multiplies two integral numbers
using recursive summation. The type should be (Integral
a) => a -> a -> a.
-}

-- 1
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0  
    where 
        go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

{-
dividedBy 15 2 = go 15 2 0 = go 13 2 1 = go 11 2 2 = go 9 2 3 = go 7 2 4 = go 5 2 5 = go 3 2 6 = go 1 2 7 = (7,1) 
-}

-- 2
recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 0 = 0
recursiveSum x = x + recursiveSum(x-1)

-- 3
multiplyBySum :: (Integral a) => a -> a -> a
multiplyBySum _ 0 = 0
multiplyBySum x y = x + multiplyBySum x (y-1)

{-
Fixing dividedBy
Our dividedBy function wasn’t quite ideal. For one thing. It was a partial function and doesn’t return a result (bottom)
when given a divisor that is 0 or less.
Using the pre-existing div function we can see how negative numbers should be handled:
Prelude> div 10 2
5
Prelude> div 10 (-2)
-5
Prelude> div (-10) (-2)
5 
Prelude> div (-10) (2)
-5

The next issue is how to handle zero. Zero is undefined for division in math, so we ought to use a datatype that lets us say
there was no sensible result when the user divides by zero. 
If you need inspiration, consider using the following datatype to handle this.
data DividedResult = Result Integer | DividedByZero
-}

data DividedResult a = Result a | DividedByZero deriving Show

dividedBy' :: Integral a => a -> a -> DividedResult a
dividedBy' num denom
  | denom == 0                  = DividedByZero
  | signum num == signum denom  = Result r
  | otherwise                   = Result (-r)
  where
    r = go (abs num) (abs denom) 0
    go n d count
      | n < d     = count
      | otherwise = go (n - d) d (count + 1)


-- McCarthy 91 function
mc91 :: Integral a => a -> a 
mc91 n 
    | n > 100 = n - 10
    | otherwise = (mc91 . mc91 . (+) 11) n
