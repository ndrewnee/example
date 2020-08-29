module Main where

import qualified HaskellByExample
import qualified Lib

main :: IO ()
main = do
  putStrLn "-------Learn you a Haskell------------\n"
  print $ Lib.myLength [1, 2, 3]
  print $ Lib.removeNonUpperCase "Some Test String"
  print $ Lib.rightTriangles
  print $ Lib.factorial 5
  print $ Lib.myHead [1, 2, 3]
  print $ Lib.myLength2 [1, 2, 3]
  print $ Lib.mySum [1, 2, 3, 4, 5, 6, 7, 8, 9]
  print $ Lib.capital "Dracula"
  print $ Lib.bmiTell 70 1.69
  print $ Lib.myMax 70 1.69
  print $ 70 `Lib.myCompare` 1.69
  print $ Lib.cylinder 70 1.69
  print $ Lib.describeList []
  print $ Lib.solveRPN "90 34 12 33 55 66 + * - + -"
  print $ Lib.solveRPN "10 10 10 10 10 sum 4 /"
  print $ Lib.optimalPath Lib.heathrowToLondon
  putStrLn "\n-------Haskell by Example------------"
  HaskellByExample.loop
  HaskellByExample.ifelse
