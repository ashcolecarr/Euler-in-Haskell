-- Project Euler Problem 6
import Data.Time (getCurrentTime, diffUTCTime)

maxNumber = 100

main = do
    start <- getCurrentTime
    putStrLn $ show (squareDifference [1..maxNumber])
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

squareDifference :: [Int] -> Int
squareDifference [] = 0
squareDifference xs = ((sum xs) ^ 2) - sumSquares xs
                      where
                          sumSquares :: [Int] -> Int
                          sumSquares [] = 0
                          sumSquares (y:ys) = (y ^ 2) + sumSquares ys  