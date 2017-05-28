-- Project Euler Problem 5

import Data.Time (getCurrentTime, diffUTCTime)

maxNum = 20

main = do
    start <- getCurrentTime
    putStr $ "The smallest number evenly divisible by each of the numbers from 1 to " ++ show maxNum ++ " is "
    putStrLn $ show (smallestDivisible [2..20]) ++ "."
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

smallestDivisible :: [Int] -> Int
smallestDivisible [] = 0
smallestDivisible xs = addUp (product primes) (product primes) (filter (\n -> n `notElem` primes) xs)
                       where
                           primes = primeSieve xs

-- Add the prime product to itself until it is divisible by every number.
addUp :: Int -> Int -> [Int] -> Int
addUp _ _ [] = 0
addUp x acc ys
    | all (\n -> x `mod` n == 0) ys = x
    | otherwise = addUp (x + acc) acc ys

-- Get the primes from a list.
primeSieve :: [Int] -> [Int]
primeSieve [] = []
primeSieve (x:xs) = x : filter (\n -> n `mod` x /= 0) (primeSieve xs)