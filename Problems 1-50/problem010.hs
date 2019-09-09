-- Project Euler Problem 10
import Data.Time (getCurrentTime, diffUTCTime)

maxNumber :: Int
maxNumber = 2000000

main = do
    start <- getCurrentTime
    putStrLn $ show (sumOfPrimes)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

sumOfPrimes :: Int
sumOfPrimes = sum $ primeSieve

primeSieve :: [Int]
primeSieve = 2 : filter (isPrime primeSieve) [3, 5..maxNumber]
    where
        isPrime :: [Int] -> Int -> Bool
        isPrime (p:ps) x = p * p > x || x `rem` p /= 0 && isPrime ps x