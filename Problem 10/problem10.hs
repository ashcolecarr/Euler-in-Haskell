-- Project Euler Problem 10

import Data.Time (getCurrentTime, diffUTCTime)

maxNumber :: Int
maxNumber = 2000000

main = do
    start <- getCurrentTime
    putStr $ "The sum of the primes below " ++ show maxNumber ++ " is "
    putStrLn $ show (sumOfPrimes) ++ "."
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."
    
sumOfPrimes :: Int
sumOfPrimes = sum $ primeSieve

-- Get the primes from a list.
primeSieve :: [Int]
primeSieve = 2 : filter (isPrime primeSieve) [3,5..maxNumber]
    where 
        isPrime :: [Int] -> Int -> Bool
        isPrime (p:ps) x = p*p > x || x `rem` p /= 0 && isPrime ps x