-- Project Euler Problem 9
import Data.Time (getCurrentTime, diffUTCTime)

tripletSum :: Int
tripletSum = 1000

main = do
    start <- getCurrentTime
    putStrLn $ show (getTriplets)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

-- Since there is exactly one triplet where a + b + c = 1000, then there should be only a single element in the list.
getTriplets :: Int
getTriplets = head [a * b * (tripletSum - (a + b)) | a <- [1..400], b <- [1..400], a ^ 2 < b ^ 2, (tripletSum - (a + b)) ^ 2 == a ^ 2 + b ^ 2]