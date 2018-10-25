import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

cleanInput :: [Char] -> [Char]
cleanInput i = map toLower $ filter (\x -> isLetter x) i

exitPalindrome :: [Char] -> IO ()
exitPalindrome s =
  if (s == reverse s) then
    do 
        putStrLn "It's a palindrome!"
        exitSuccess
  else
    do
        putStrLn "Nope!" 
        return ()

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    exitPalindrome $ cleanInput line1