-- Project Euler Problem 37
import Data.Time (getCurrentTime, diffUTCTime)

main = do
    start <- getCurrentTime
    putStrLn $ show (primeSum)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

primeSum :: Int
primeSum = sum $ take numberOfTruncatablePrimes $ filter (isTruncatablePrime) $ dropWhile (< 11) primes 
    where
        numberOfTruncatablePrimes :: Int
        numberOfTruncatablePrimes = 11 -- There are only 11 truncatable primes.

isTruncatablePrime :: Int -> Bool
isTruncatablePrime prime = leftTruncated prime (leftTruncator prime) && rightTruncated prime
    where
        leftTruncator :: Int -> Int
        leftTruncator num = 10 ^ (((length . show) num) - 1)
        leftTruncated :: Int -> Int -> Bool
        leftTruncated _ 1 = True
        leftTruncated candidate truncator
            | isPrime (candidate `rem` truncator) = leftTruncated (candidate `rem` truncator) (truncator `quot` 10)
            | otherwise = False
        rightTruncated :: Int -> Bool
        rightTruncated 0 = True
        rightTruncated candidate
            | isPrime candidate = rightTruncated (candidate `quot` 10)
            | otherwise = False

isPrime :: Int -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) /= 0 && r)) True primes

primes :: [Int]
primes = 2 : filter (isPrime) [3, 5..]
 