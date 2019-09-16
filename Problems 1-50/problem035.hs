-- Project Euler Problem 35
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

main = do
    start <- getCurrentTime
    putStrLn $ show (circularPrimeCount)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

circularPrimeCount :: Int
circularPrimeCount = length $ filter (isCircularPrime) primeStrings

isCircularPrime :: String -> Bool
isCircularPrime xs = rotate xs xs
    where
        rotate :: String -> String -> Bool
        rotate (x:xs) ys
            | xs ++ [x] == ys = True
            | isPrime $ read (xs ++ [x]) = rotate (xs ++ [x]) ys
            | otherwise = False

primeStrings :: [String]
primeStrings = map (show) primes

isPrime :: Int -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) /= 0 && r)) True primes

primes :: [Int]
primes = 2 : filter (isPrime) [3, 5..maxNumber]
    where
        maxNumber :: Int
        maxNumber = 1000000