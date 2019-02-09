-- http://learnyouahaskell.com/starting-out

module StartingOut
    ( myLength
    , removeNonUpperCase
    , rightTriangles
    )
where

myLength :: (Num b) => [a] -> b
myLength xs = sum [ 1 | _ <- xs ]

removeNonUpperCase :: String -> String
removeNonUpperCase st = [ c | c <- st, c `elem` ['A' .. 'Z'] ]

rightTriangles =
    [ (a, b, c)
    | c <- [1 .. 10]
    , b <- [1 .. c]
    , a <- [1 .. b]
    , a ^ 2 + b ^ 2 == c ^ 2
    ]
