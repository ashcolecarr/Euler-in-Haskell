-- Project Euler Problem 6

import Data.Time (getCurrentTime, diffUTCTime)

maxNum = 100

main = do
    start <- getCurrentTime
    putStr $ "The difference between the sum of the squares of the first " ++ show maxNum ++ " numbers and the square of the sum is "
    putStrLn $ show (squareDifference [1..maxNum]) ++ "."
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

squareDifference :: [Int] -> Int
squareDifference [] = 0
squareDifference xs = ((sum xs) ^ 2) - sumSquares xs
                      where
                          sumSquares :: [Int] -> Int
                          sumSquares [] = 0
                          sumSquares (y:ys) = (y ^ 2) + sumSquares ys