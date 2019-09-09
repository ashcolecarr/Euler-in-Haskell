-- Project Euler Problem 1
import Data.Time (getCurrentTime, diffUTCTime)

maximumValue = 1000

main = do
    start <- getCurrentTime

    putStrLn $ show (multiplesSum [3..(maximumValue - 1)])
    stop <- getCurrentTime
    putStrLn $ "\nProgram execution took " ++ show (diffUTCTime stop start) ++ " seconds."

multiplesSum :: [Int] -> Int
multiplesSum xs = sum [x | x <- xs, x `mod` 3 == 0 || x `mod` 5 == 0]