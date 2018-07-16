module OperatingSystems where

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem , lang :: ProgLang }
    deriving (Eq, Show)

{-
Write a function that generates all possible values of Programmer.
Use the provided lists of inhabitants of OperatingSystem and ProgLang.
-}

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]


-- zipWith is obviously not exhaustive
allProgrammers' :: [Programmer]
allProgrammers' = zipWith (Programmer) allOperatingSystems allLanguages 

-- Using list comprehension. The best way, imho. We don't need tp remove duplicates.
allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages ]


