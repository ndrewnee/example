module Main where

import           StartingOut
import           TypesAndTypeClasses

main :: IO ()
main = do
    print $ myLength [1, 2, 3]
    print $ removeNonUpperCase "Some Test String"
    print $ rightTriangles
    print $ factorial 5
