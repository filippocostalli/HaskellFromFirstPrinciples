module LanguageExrcise where


import Data.Char

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph x = unwords . map capitalizeWord $ words x