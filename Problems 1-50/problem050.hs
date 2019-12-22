-- Project Euler Problem 50
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (maximumBy)
import Data.Function (on)

main = do
    start <- getCurrentTime
    putStrLn $ show (largestConsecutivePrime)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

largestConsecutivePrime :: Int
largestConsecutivePrime = head $ maximumBy (compare `on` length) $ map (\x -> checkPrimeSum x) primes
    where
        checkPrimeSum :: Int -> [Int]
        checkPrimeSum x = dropWhile (not . isPrime) $ reverse $ cumulativePrimeSums x

cumulativePrimeSums :: Int -> [Int]
cumulativePrimeSums x = takeWhile (< maxNumber) $ tail $ scanl (+) 0 $ dropWhile (< x) primes

isPrime :: Int -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) /= 0 && r)) True primes

primes :: [Int]
primes = 2 : filter (isPrime) [3, 5..maxNumber]
        
maxNumber :: Int 
maxNumber = 1000000

