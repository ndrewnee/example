module Main where

import           StartingOut

main :: IO ()
main = do
    print $ myLength [1, 2, 3]
    print $ removeNonUpperCase "Some Test String"
    print $ rightTriangles
