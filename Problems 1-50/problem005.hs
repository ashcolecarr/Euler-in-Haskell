-- Project Euler Problem 5
import Data.Time (getCurrentTime, diffUTCTime)

maxNumber = 20

main = do
    start <- getCurrentTime
    putStrLn $ show (smallestDivisible [2..maxNumber])
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

smallestDivisible :: [Int] -> Int
smallestDivisible [] = 0
smallestDivisible xs = addPrimeProduct (product primes) (product primes) (filter (\n -> n `notElem` primes) xs)
                       where
                           primes = primeSieve xs
                           
-- Add the prime product to itself until it is divisible by every number.
addPrimeProduct :: Int -> Int -> [Int] -> Int
addPrimeProduct _ _ [] = 0
addPrimeProduct x acc ys
    | all (\n -> x `mod` n == 0) ys = x
    | otherwise = addPrimeProduct (x + acc) acc ys

primeSieve :: [Int] -> [Int]
primeSieve [] = []
primeSieve (x:xs) = x : filter (\n -> n `mod` x /= 0) (primeSieve xs)















