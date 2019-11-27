-- Project Euler Problem 45
import Data.Time (getCurrentTime, diffUTCTime)

main = do
    start <- getCurrentTime
    putStrLn $ show (tphNumber triangleNumbers)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

tphNumber :: [Int] -> Int
tphNumber (x:xs) 
    | isPentagonal x && isHexagonal x = x
    | otherwise = tphNumber xs

triangleNumbers :: [Int]
triangleNumbers = [calculateTriangle x | x <- [(start + 1)..]]
    where
        -- The problem already gives a number that satisfies the condition.
        start :: Int
        start = 285
        calculateTriangle :: Int -> Int
        calculateTriangle n = (n * (n + 1)) `quot` 2

isPentagonal :: Int -> Bool
isPentagonal x = (fromIntegral $ truncate $ pentagonalTest $ fromIntegral x) == (pentagonalTest $ fromIntegral x)
    where
        pentagonalTest :: Floating a => a -> a
        pentagonalTest n = ((sqrt (24 * n + 1)) + 1) / 6

isHexagonal :: Int -> Bool
isHexagonal x = (fromIntegral $ truncate $ hexagonalTest $ fromIntegral x) == (hexagonalTest $ fromIntegral x)
    where
        hexagonalTest :: Floating a => a -> a
        hexagonalTest n = ((sqrt (8 * n + 1)) + 1) / 4