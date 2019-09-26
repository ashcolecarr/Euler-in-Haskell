-- Project Euler Problem 41
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Set (fromList, toList)
import Data.Char (digitToInt)

main = do
    start <- getCurrentTime
    putStrLn $ show (largestPandigitalPrime)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

largestPandigitalPrime :: Int
largestPandigitalPrime = maximum [x | x <- dropWhile (< minNumber) primes, isPandigital ((toList . fromList . digits) x) 1]
    where
        -- The problem already gives this number so it cannot be lower.
        minNumber :: Int
        minNumber = 2143

isPandigital :: [Int] -> Int -> Bool
isPandigital [] _ = True
isPandigital (x:xs) y
    | x == 0 = False
    | x == y = isPandigital xs (y + 1)
    | otherwise = False

isPrime :: Int -> Bool
isPrime n = n > 1 && foldr (\p r -> p * p > n || ((n `rem` p) /= 0 && r)) True primes

primes :: [Int]
primes = 2 : filter (isPrime) [3, 5..maxNumber]
    where
        -- Pandigital numbers are divisible by 3 except for 1234 and 1234567.
        maxNumber :: Int
        maxNumber = 7654321

digits :: Int -> [Int]
digits digit = reverse $ digits' digit
    where
        digits' :: Int -> [Int]
        digits' 0 = []
        digits' num = num `rem` 10 : digits' (num `quot` 10)