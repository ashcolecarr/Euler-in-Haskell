-- Project Euler Problem 21
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (foldl')

maxNumber :: Int
maxNumber = 10000

main = do
    start <- getCurrentTime
    putStrLn $ show (foldl' amicables 0 [1..maxNumber])
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

getSum :: Int -> Int
getSum number = sum $ divisors number

divisors :: Int -> [Int]
divisors number = 1 : (concatMap (\n -> [n, if n * n == number then 0 else number `quot` n]) $ filter (\n -> number `rem` n == 0) $ takeWhile (\n -> n * n <= number) [2..number])

amicables :: Int -> Int -> Int
amicables acc number
    | number == amicable = acc
    | number == getSum (amicable) = acc + number
    | otherwise = acc
    where
        amicable = getSum number