module StartingOut
    ( myLength
    , removeNonUpperCase
    , rightTriangles
    )
where

-- http://learnyouahaskell.com/starting-out#im-a-list-comprehension

myLength :: [a] -> Integer
myLength xs = sum [ 1 | _ <- xs ]

removeNonUpperCase :: String -> String
removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- http://learnyouahaskell.com/starting-out#tuples

rightTriangles = [ (a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
