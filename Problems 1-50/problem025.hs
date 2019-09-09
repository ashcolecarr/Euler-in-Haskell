-- Project Euler Problem 25
import Data.Time (getCurrentTime, diffUTCTime)

main = do
    start <- getCurrentTime
    putStrLn $ show (length $ fibonacci [1, 1])
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

maxDigits :: Integer
maxDigits = 1000

fibonacci :: [Integer] -> [Integer]
fibonacci (x:y:ys)
    | x >= thousandDigits = (x : y : ys)
    | otherwise = fibonacci $ (x + y) : x : y : ys

thousandDigits :: Integer
thousandDigits = 10 ^ (maxDigits - 1)