-- Project Euler Problem 23
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Array

main = do
    start <- getCurrentTime
    putStrLn $ show (sum [1..limit] - sum sumOfAbundants)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

-- Note: the lower bound for numbers that can all be expressed as the sum of
-- two abundant numbers is 20161 rather than 28123.
limit :: Int
limit = 20161

sumOfAbundants :: [Int]
sumOfAbundants = filter (any (abundantsMemo !) . bothAbundant) [1..limit]
    where
        bothAbundant :: Int -> [Int]
        bothAbundant num = map (num-) $ takeWhile (<= num `quot` 2) abundants

abundants :: [Int]
abundants = filter (abundantsMemo !) [1..limit]

abundantsMemo :: Array Int Bool
abundantsMemo = listArray (1, limit) $ map isAbundant [1..limit]

isAbundant :: Int -> Bool
isAbundant number = number < (sum $ divisors number)

-- If conditions verifies if number is a perfect square, otherwise it will erroneously return duplicate divisors.
divisors :: Int -> [Int]
divisors number = 1 : (concatMap (\n -> [n, if n * n == number then 0 else number `quot` n]) $ filter (\n -> number `rem` n == 0) $ takeWhile (\n -> n * n <= number) [2..number])