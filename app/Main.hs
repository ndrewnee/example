module Main where

import           Lib

main :: IO ()
main = do
    print $ myLength [1, 2, 3]
    print $ removeNonUpperCase "Some Test String"
    print $ rightTriangles
    print $ factorial 5
    print $ solveRPN "90 34 12 33 55 66 + * - + -"
    print $ solveRPN "10 10 10 10 10 sum 4 /"
    print $ optimalPath heathrowToLondon
