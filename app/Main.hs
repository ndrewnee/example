module Main where

import           Lib

main :: IO ()
main = do
    print $ myLength [1, 2, 3]
    print $ removeNonUpperCase "Some Test String"
    print $ rightTriangles
    print $ factorial 5
    print $ myHead [1, 2, 3]
    print $ myLength2 [1, 2, 3]
    print $ mySum [1, 2, 3, 4, 5, 6, 7, 8, 9]
    print $ capital "Dracula"
    print $ bmiTell 70 1.69
    print $ myMax 70 1.69
    print $ 70 `myCompare` 1.69
    print $ cylinder 70 1.69
    print $ describeList []
    print $ solveRPN "90 34 12 33 55 66 + * - + -"
    print $ solveRPN "10 10 10 10 10 sum 4 /"
    print $ optimalPath heathrowToLondon
