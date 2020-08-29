module HaskellByExample
  ( loop,
    ifelse,
  )
where

import Control.Monad.Cont

loop :: IO ()
loop = do
  putStrLn "\n[For Loops]\n"

  forM_ [1 .. 3] $ \i -> do
    print i

  forM_ [7 .. 9] $ \j -> do
    print j

  withBreak $ \break ->
    forM_ [1 ..] $ \_ -> do
      p "loop"
      break ()
  where
    withBreak = (`runContT` return) . callCC
    p = liftIO . putStrLn

ifelse :: IO ()
ifelse = do
  putStrLn "\n[If/Else]\n"

  if 7 `mod` 2 == 0
    then putStrLn "7 is even"
    else putStrLn "7 is odd"

  if 8 `mod` 4 == 0
    then putStrLn "8 is divisible by 4"
    else return ()

  let num = 9
  putStrLn $
    if num < 0
      then show num ++ " is negative"
      else
        if num < 10
          then show num ++ " has 1 digit"
          else show num ++ " has multiple digits"
