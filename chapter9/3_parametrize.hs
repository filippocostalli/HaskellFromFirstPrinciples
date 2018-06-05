
myStrings :: Char -> String -> [String]
myStrings _ [] = []
myStrings c x = ( takeWhile (/= c) x ) : ( myStrings  c . drop 1 $ dropWhile (/= c) x )