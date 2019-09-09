-- Project Euler Problem 27
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (maximumBy)
import Data.Function (on)

main = do
    start <- getCurrentTime
    putStrLn $ show (maximumProduct)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

minNumber :: Int
minNumber = -1000

maxNumber :: Int
maxNumber = 1000

-- The term b must result in a prime at n = 0, since n ^ 2 + an + b = b,
-- so that eliminates negative values and any nonprime values for b.
-- The term a must be odd since b must be prime and must be even if b = 2.
coefficients :: [(Int, Int)]
coefficients = [(a, b) | a <- filter (odd) [minNumber + 1..maxNumber], b <- filter (isPrime) [0..maxNumber]]

primeNumberSequences :: (Int, Int) -> (Int, Int)
primeNumberSequences (a, b) = (length $ takeWhile (isPrime) [abs $ n ^ 2 + a * n + b | n <- [0..]], a * b)

maximumProduct :: Int
maximumProduct = snd $ maximumBy (compare `on` fst) $ map (primeNumberSequences) coefficients

factors :: Int -> [Int]
factors m = factor m (head primes) (tail primes)
    where
        factor :: Int -> Int -> [Int] -> [Int]
        factor m n ns
            | m < 2 = []
            | m < n ^ 2 = [m]
            | m `rem` n == 0 = n : factor (m `quot` n) n ns
            | otherwise = factor m (head ns) (tail ns)

primes :: [Int]
primes = 2 : filter (\n -> head (factors n) == n) [3, 5..]

isPrime :: Int -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) /= 0 && r)) True primes
