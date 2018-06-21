module ChapterExercisesWarmUp where

{-
1. Given the following sets of consonants and vowels:
stops = "pbtdkg"
vowels = "aeiou"

a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. 
These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.

b) Modify that function so that it only returns the combinations that begin with a p.

c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.
-}
stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

-- 1.
swsCombinations:: [Char] -> [Char] -> [(Char, Char, Char)]
swsCombinations xs ys = [ (a, b, c) | a <- xs, b <- ys, c <- xs ]

-- 2.
getBeginsP :: (Char, Char, Char) -> Bool
getBeginsP (a, _, _) 
    | a == 'p' = True
    | otherwise = False

swsCombinations':: [Char] -> [Char] -> [(Char, Char, Char)]
swsCombinations' xs ys = filter getBeginsP (swsCombinations xs ys)

-- using lambdas
swsCombinations'':: [Char] -> [Char] -> [(Char, Char, Char)]
swsCombinations'' xs ys = filter (\(a, _, _) -> a == 'p') (swsCombinations xs ys)

-- 3.
nounsVerbsCombinations:: [[Char]] -> [[Char]] -> [[Char]]
nounsVerbsCombinations xs ys = [ a ++ " " ++ b ++ " " ++ c | a <- xs, b <- ys, c <- xs ]

nouns :: [[Char]]
nouns = ["Cat", "Plumber", "Tube"]

verbs :: [[Char]]
verbs = ["eats", "fixes", "scratches"]

{-
2. What does the following mystery function do? What is its type? 
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))
-}

{-
The funcion takes a string 's' and:
- applies function words on it -> returns a list of strings, obtained breaking up the original string using space as a break character
- applies map tho the returned list of string, using length as a mapping funcion -> returns a list of Int, whose elements are the lenghts of the words contained in string 's'
- applies sum to the returned lis -> returns the sum of length of the words contained in string 's'. We're caling it 'numberOfCharactersExceptSpace'

Then:
- applies function words on 's'' -> returns a list of strings, obtained breaking up the original string using space as a break character
- applies lenght to it -> returns the number of words contained in 's'. We're calling it 'numberOfWords'

Then div numberOfCharactersExceptSpace numberOfWords

So the function returns the rounded average of characters per word 
-}
seekritFunc ::  [Char] -> Int
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))

{-
3. Rewrite  using fractional division
-}

seekritFunc' ::  [Char] -> Double
seekritFunc' x =
    fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))