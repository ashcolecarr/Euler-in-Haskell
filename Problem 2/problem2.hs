-- Project Euler Problem 2
import Data.Time (getCurrentTime, diffUTCTime)

limit = 4000000

main = do
    start <- getCurrentTime
    
    putStrLn $ "The sum of the even fibonacci terms below " ++ show limit ++ " is " ++ show (sum (filter even (fibonacci [1, 1]))) ++ "."
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

fibonacci :: [Int] -> [Int]
fibonacci (x:y:ys)
  | x + y >= limit = x : y : ys
  | otherwise = fibonacci ((x + y) : x : y : ys)