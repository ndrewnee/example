
module Lib
    ( myLength
    , removeNonUpperCase
    , rightTriangles
    , factorial
    , solveRPN
    , heathrowToLondon
    , optimalPath
    )
where

-- http://learnyouahaskell.com/starting-out

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

-- http://learnyouahaskell.com/types-and-typeclasses

factorial :: Integer -> Integer
factorial n = product [1 .. n]

-- http://learnyouahaskell.com/functionally-solving-problems

-- Reverse Polish notation calculator
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "*"          = (x * y) : ys
    foldingFunction (x : y : ys) "+"          = (x + y) : ys
    foldingFunction (x : y : ys) "-"          = (y - x) : ys
    foldingFunction (x : y : ys) "/"          = (y / x) : ys
    foldingFunction (x : y : ys) "^"          = (y ** x) : ys
    foldingFunction (x     : xs) "ln"         = log x : xs
    foldingFunction xs           "sum"        = [sum xs]
    foldingFunction xs           numberString = read numberString : xs

-- Heathrow to London
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon =
    [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA          = sum $ map snd pathA
        priceB          = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossPriceToA   = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB   = priceA + a + c
        newPathToA      = if forwardPriceToA <= crossPriceToA
            then (A, a) : pathA
            else (C, c) : (B, b) : pathB
        newPathToB = if forwardPriceToB <= crossPriceToB
            then (B, b) : pathB
            else (C, c) : (A, a) : pathA
    in  (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath