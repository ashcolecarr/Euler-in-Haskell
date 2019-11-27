-- Project Euler Problem 44
import Data.Time (getCurrentTime, diffUTCTime)

main = do
    start <- getCurrentTime
    putStrLn $ show (getPentagonals listPentagonals)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

getPentagonals :: [Int] -> Int
getPentagonals (x:xs)
    | checkPentagonals x (pentagonalSublist x) > -1 = checkPentagonals x (pentagonalSublist x)
    | otherwise = getPentagonals xs
    where
        pentagonalSublist :: Int -> [Int]
        pentagonalSublist x = takeWhile (< x) listPentagonals
        checkPentagonals :: Int -> [Int] -> Int
        checkPentagonals _ [] = -1
        checkPentagonals x (y:ys)
            | isPentagonal (x + y) && isPentagonal (x - y) = abs (x - y)
            | otherwise = checkPentagonals x ys

listPentagonals :: [Int]
listPentagonals = [calculatePentagonal x | x <- [1..]]
    where
        calculatePentagonal :: Int -> Int
        calculatePentagonal n = n * (3 * n - 1) `quot` 2

isPentagonal :: Int -> Bool
isPentagonal x = (fromIntegral $ truncate $ pentagonalTest $ fromIntegral x) == (pentagonalTest $ fromIntegral x)
    where
        pentagonalTest :: Floating a => a -> a
        pentagonalTest n = ((sqrt (24 * n + 1)) + 1) / 6