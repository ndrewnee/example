module HaskellByExample
  ( loop,
    ifelse,
    switch,
    arrays,
  )
where

import qualified Control.Monad.Cont as Cont
import qualified Data.Array as Array
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as WeekDate

loop :: IO ()
loop = do
  putStrLn "\n[For Loops]\n"

  Cont.forM_ [1 .. 3] $ \i -> do
    print i

  Cont.forM_ [7 .. 9] $ \j -> do
    print j

  withBreak $ \break ->
    Cont.forM_ [1 ..] $ \_ -> do
      p "loop"
      break ()
  where
    withBreak = (`Cont.runContT` return) . Cont.callCC
    p = Cont.liftIO . putStrLn

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

switch :: IO ()
switch = do
  putStrLn "\n[Switch]\n"

  let i = 2
  putStrLn $ "write " ++ show i ++ " as "
  case i of
    1 -> putStrLn "one"
    2 -> putStrLn "two"
    3 -> putStrLn "three"

  now <- Time.getCurrentTime
  let (_, _, week) = WeekDate.toWeekDate . Time.utctDay $ now
  putStrLn $
    case week of
      6 -> "it's the weekend"
      7 -> "it's the weekend"
      _ -> "it's a weekday"

  localTime <- Time.utcToLocalZonedTime now
  let hour = Time.todHour . Time.localTimeOfDay . Time.zonedTimeToLocalTime $ localTime
  case hour of
    _
      | hour < 12 -> putStrLn "it's before noon"
      | otherwise -> putStrLn "it's after noon"

arrays :: IO ()
arrays = do
  putStrLn "\n[Arrays]\n"

  let a = Array.array (0, 4) [(i, 0) | i <- [0 .. 4]]
  putStrLn $ "emp: " ++ show a

  let a' = a Array.// [(4, 100)]
  putStrLn $ "set: " ++ show a'
  putStrLn $ "get: " ++ show (a' Array.! 4)
  putStrLn $ "len: " ++ show ((+ 1) . snd . Array.bounds $ a')

  let b = Array.array (0, 4) [(i, i + 1) | i <- [0 .. 4]]
  putStrLn $ "dcl: " ++ show b

  let twoD = Array.array ((0, 0), (1, 2)) [((i, j), i + j) | i <- [0 .. 1], j <- [0 .. 2]]
  putStrLn $ "2d: " ++ show twoD
