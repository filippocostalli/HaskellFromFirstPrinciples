
multiple3 :: [Integer] -> [Integer]
multiple3 xs = filter (\x -> mod x 3 == 0) xs


countMultiple3 :: [Integer] -> Int
countMultiple3 xs = length $ multiple3 xs


-- Used to RemoveArticles
myWords :: String -> [String]
myWords [] = []
myWords x = ( takeWhile (/=' ') x ) : ( myWords . drop 1 $ dropWhile (/=' ') x )


removeArticles :: String -> [String]
removeArticles xs = filter (\x -> notElem x ["the", "an", "a"]) $ myWords xs