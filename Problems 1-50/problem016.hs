-- Project Euler Problem 16
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

power :: Int
power = 1000

main = do
    start <- getCurrentTime
    putStrLn $ show (digitSum)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

digitSum :: Int
digitSum = sum $ map (digitToInt) $ show largeNumber

largeNumber :: Integer
largeNumber = 2 ^ power