-- Project Euler Problem 28
import Data.Time (getCurrentTime, diffUTCTime)

main = do
    start <- getCurrentTime
    putStrLn $ show (sum $ grid [1] 2 0)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

gridSize :: Int
gridSize = 1001

-- The maximum number of the grid will be the grid size squared.
grid :: [Int] -> Int -> Int -> [Int]
grid (x:xs) y z
    | x == gridSize ^ 2 = x : xs
    | z == 3 = grid ((x + y) : x : xs) (y + 2) 0
    | otherwise = grid ((x + y) : x : xs) y (z + 1)