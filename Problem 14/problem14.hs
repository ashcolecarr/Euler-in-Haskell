-- Project Euler Problem 14

import Data.Time (getCurrentTime, diffUTCTime)
import Data.Array

maxNumber :: Int
maxNumber = 999999

main = do
  start <- getCurrentTime
  putStr $ "The starting number under " ++ show (maxNumber + 1) ++ " with the longest Collatz chain is "
  putStrLn $ show (snd $ maximum $ map (\n -> (collatz n, n)) [1..maxNumber]) ++ "."
  stop <- getCurrentTime
  putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."
  
memoization :: Array Int Int
memoization = listArray (1, size) $ map (collatz) [1..size]
  where
    size = maxNumber

collatz :: Int -> Int
collatz number
  | number == 1 = 0
  | inRange (bounds memoization) nextLink = 1 + memoization ! nextLink
  | otherwise = 1 + collatz nextLink
  where
    nextLink :: Int
    nextLink
      | number == 1 = 0
      | even number = number `div` 2
      | otherwise = (3 * number) + 1