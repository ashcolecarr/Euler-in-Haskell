-- Project Euler Problem 27

import Data.Time (getCurrentTime, diffUTCTime)

main = do
  start <- getCurrentTime
  putStr $ "The product of a and b that produces the maximum number of primes for consecutive values of n is "
  putStrLn $ show () ++ "."
  stop <- getCurrentTime
  putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."
  
minNumber :: Int
minNumber = -1000

maxNumber :: Int
maxNumber = 1000

-- The term b must result in a prime at n = 0, since n^2 + an + b = b, so that eliminates negative values and any nonprime values for b.
coefficients :: [(Int, Int)]
coefficients = [(a, b) | a <- [minNumber..maxNumber], b <- filter (isPrime) [0..maxNumber]]

primeNumber :: (Int, Int) -> Int
primeNumber (a, b) = length $ takeWhile (isPrime) [n ^ 2 + a * n + b | n <- [0..]]

maximumSequence :: [Int] -> Int
maximumSequence xs = 

factors :: Int -> [Int]
factors m = factor m (head primes) (tail primes) 
  where
    factor m n ns
      | m < 2 = []
      | m < n ^ 2 = [m]
      | m `mod` n == 0 = n : factor (m `div` n) n ns
      | otherwise = factor m (head ns) (tail ns)

isPrime :: Int -> Bool
isPrime n = n > 1 && head (factors n) == n






















termList :: [(Int, Int)]
termList = [(a, b) | a <- [minNum..maxNum], b <- filter (isPrime) [0..maxNum]]

                    
isPrime :: Int -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) /= 0 && r))

primes :: [Int]
primes = 2 : filter (\n-> head (factors n) == n) [3,5..]

minNum :: Int
minNum = -999

maxNum :: Int
maxNum = 999