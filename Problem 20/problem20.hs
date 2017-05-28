-- Project Euler Problem 20

import Data.Time (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

factorial :: Integer
factorial = 100

main = do
  start <- getCurrentTime
  putStr $ "The sum of the digits of the number " ++ show factorial ++ "! is "
  putStrLn $ show (digitSum) ++ "."
  stop <- getCurrentTime
  putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

digitSum :: Int
digitSum = sum $ map (digitToInt) $ show largeNumber

largeNumber :: Integer
largeNumber = product [1..factorial]