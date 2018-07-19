module Phone where

import Data.Char
import Data.List


type Digit = Char

data PhoneButton = PhoneButton Digit String deriving Show

data DaPhone = DaPhone [PhoneButton] deriving Show

-- Valid presses: 1 and up
type Presses = Int

myButtons :: [PhoneButton]
myButtons = 
    [ PhoneButton '1' "", 
      PhoneButton '2' "ABC",
      PhoneButton '3' "DEF",
      PhoneButton '4' "GHI", 
      PhoneButton '5' "JKL", 
      PhoneButton '6' "MNO", 
      PhoneButton '7' "PQRS", 
      PhoneButton '8' "TUV", 
      PhoneButton '9' "WXYZ",
      PhoneButton '*' "^",
      PhoneButton '0' "+ _",
      PhoneButton '#' ".,"
    ]

myPhone :: DaPhone
myPhone = DaPhone  myButtons

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

containsChar :: Char -> PhoneButton -> Bool
containsChar ch (PhoneButton d s) = elem (toUpper ch) (d:s)

selectButton :: [PhoneButton] -> Char -> PhoneButton
selectButton phoneButtons ch = head $  filter (containsChar ch) phoneButtons

getPresses :: PhoneButton -> Char -> [(Digit, Presses)]
getPresses (PhoneButton digit chars) ch 
    | isDigit ch = [(digit, 1 + length chars)]
    | isUpper ch = ('*', 1) : getPresses (PhoneButton digit chars) (toLower ch)
    | otherwise = [ (digit, (length $ takeWhile ( \x -> x /= toUpper ch)  chars) + 1)]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone phoneButtons) ch = getPresses (selectButton phoneButtons ch) ch
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead _ [] = []
cellPhonesDead phone (x:xs) = (reverseTaps phone x) ++ cellPhonesDead phone xs

-- pressesConversation convo
pressesConversation :: [String] -> [[(Digit, Presses)]]
pressesConversation s = map (cellPhonesDead myPhone) s

---------------------------
-- two versions : one using foldr, one recursion
fingertaps :: [(Digit, Presses)] -> Presses
fingertaps = foldr (\(_, n) x -> x + n) 0

fingertaps' :: [(Digit, Presses)] -> Presses
fingertaps' [] = 0
fingertaps'( (_ , p):xs ) = p + fingertaps' xs

-- using foldr but more elegant and concise
fingertaps'' :: [(Digit, Presses)] -> Presses
fingertaps'' = foldr ((+) . snd) 0

--------------
mostFrequent :: Ord a => [a] -> (Int, a)
mostFrequent = maximum . map (\l -> (length l, head l)) . group . sort

mostPopularLetter :: String -> Char
mostPopularLetter = snd . mostFrequent . map toLower . filter isLetter

-----------------------------

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . unwords


coolestWord :: [String] -> String
coolestWord = snd . mostFrequent . map (id .  map (\ s -> toLower s))
-- reduce all string to lowercase
-- get most frequant in the list of string
-- get letter
