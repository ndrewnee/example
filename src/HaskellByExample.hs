module HaskellByExample
  ( loop,
    ifelse,
    switch,
    arrays,
    slices,
    maps,
    range,
    closures,
  )
where

import qualified Control.Monad.Cont as Cont
import qualified Data.Array as Array
import qualified Data.IORef as IORef
import qualified Data.Map as Map
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

slices :: IO ()
slices = do
  putStrLn "\n[Slices]\n"

  let s = [] :: [String]
  putStrLn $ "emp: " ++ show s

  let s' = ["a", "b", "c"]
  putStrLn $ "set: " ++ show s'
  putStrLn $ "get: " ++ s' !! 2
  putStrLn $ "len: " ++ show (length s')

  let s2 = s' ++ ["d"]
  let s3 = s2 ++ ["d", "f"]
  putStrLn $ "apd: " ++ show s3

  let c = s3
  putStrLn $ "cpy: " ++ show c

  let l1 = drop 2 . take 5 $ s3
  putStrLn $ "sl1: " ++ show l1

  let l2 = take 5 $ s3
  putStrLn $ "sl2: " ++ show l2

  let l3 = drop 2 s3
  putStrLn $ "sl3: " ++ show l3

  let t = ["g", "h", "i"]
  putStrLn $ "dcl: " ++ show t

  let twoD = [[i + j | j <- [0 .. i]] | i <- [0 .. 2]]
  putStrLn $ "2d: " ++ show twoD

maps :: IO ()
maps = do
  putStrLn "\n[Maps]\n"

  let m0 = Map.empty
  let m1 = Map.insert "k1" 7 m0
  let m = Map.insert "k2" 13 m1
  putStrLn $ "map: " ++ show m

  let v1 = m Map.! "k1"
  putStrLn $ "v1: " ++ show v1
  putStrLn $ "len: " ++ show (Map.size m)

  let m' = Map.delete "k2" m
  putStrLn $ "map: " ++ show m'

  let prs = Map.lookup "k2" m'
  putStrLn $ "prs: " ++ show prs

  let n = Map.fromList [("foo", 1), ("bar", 2)]
  putStrLn $ "map: " ++ show n

range :: IO ()
range = do
  putStrLn "\n[Range]\n"

  let nums = [2, 3, 4]
  putStrLn $ "sum: " ++ show (sum nums)

  mapM_ putStrLn ["index: " ++ show i | (i, num) <- zip [0 ..] nums, num == 3]

  let kvs = Map.fromList [("a", "apple"), ("b", "banana")]
  Cont.forM_ (Map.toList kvs) $ \(k, v) -> putStrLn $ k ++ " -> " ++ v

  mapM_ print $ zip [0 ..] "haskell"

intSeq :: IORef.IORef Int -> IO Int
intSeq ref = do
  IORef.modifyIORef' ref (+ 1)
  IORef.readIORef ref

closures :: IO ()
closures = do
  putStrLn "\nClosures\n"

  ref <- IORef.newIORef 0
  let nextInt = intSeq ref

  print =<< nextInt
  print =<< nextInt
  print =<< nextInt

  ref' <- IORef.newIORef 0
  let newInts = intSeq ref'
  print =<< newInts
