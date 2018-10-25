module Main where

import Control.Monad (forever)    -- [1] Basically it allows us to execute a function over and over again, infinitely, or until we, instead of evaluating once and then stopping.
import Data.Char (toLower)        -- [2] to convert all characters of our string to lowercase:
import Data.Maybe (isJust)        -- [3] We will use isJust from Data.Maybe to determine if every character in our puzzle has been discovered already or not
                                  -- all is a function which answers the question,“given a function that will return True or False for each element, does it return True for all of them?”
import Data.List (intersperse)    -- [4] intersperse elements in a list. intersperse '*' "Blah" = "B*l*a*h"
import System.Exit (exitSuccess)  -- [5] We use exitSuccess from System.Exit to exit successfully — no errors, we’re simply done.
import System.Random (randomRIO)  -- [6] We use randomRIO from System.Random to select a word from our dictionary at random. System.Random is in the library random.


minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

{-
type WordList = [String] -- For clarity’s sake, we’re using a type synonym to declare what we mean by [String] in our types.

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

-- take the output of allWords and filter it to fit the length criteria we defined above.
-- That will give us a shorter list of words to use in the puzzles:
gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where
    gameLength w =
      let
        l = length (w :: String)
      in
        l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0 , length wl - 1)
  return $ wl !! randomIndex
-}

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)


gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where 
    gameLength w = 
      let 
        l = length (w :: String) 
      in  
        l > minWordLength && l < maxWordLength



randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


--1. the word we’re trying to guess
--2. the characters we’ve filled in so far
--3. the letters we’ve guessed so far
data Puzzle = Puzzle String [Maybe Char] [Char]

-- instance of the typeclass Show for our datatype Puzzle
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed


freshPuzzle :: String -> Puzzle
freshPuzzle x = Puzzle x (listOfNothing x) []
    where listOfNothing a = map (\i -> Nothing) a



--Now we need a function that looks at the Puzzle String and
--determines whether the character you guessed is an element of that string. 
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ ) c = elem c w

-- The next function is very similar to the one you just wrote, but this time we don’t care if the Char is part of the String argument 
-- this time we want to check and see if it is an element of the guessed list.
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ ag) c = elem c ag


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

--- the following twos are the core -----------------------
zipper :: Char -> Char -> Maybe Char -> Maybe Char
zipper guessed wordChar guessChar =
  if wordChar == guessed
  then Just wordChar
  else guessChar

  -- zipper c => partially applied to the guessed char so signature is Char -> Maybe Char -> Maybe Char suit for zipWith
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where 
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

-- ----------

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

-- added this to get number of correct guess
nOfCorrectGuess :: [Maybe Char] -> Int
nOfCorrectGuess x = length $ filter isJust x
  where isJust a = case a of 
                    Nothing -> False
                    Just _ -> True


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledInSoFar guessed) =
  --only incorrect guesses count against you, so you can make as many correct guesses as you need to fill in the word.
  -- if (length guessed) > 7 then -- code replaced
  if (length guessed - nOfCorrectGuess filledInSoFar) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame 
    _   -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
