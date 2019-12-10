-- Project Euler Problem 48
import Data.Time (getCurrentTime, diffUTCTime)

main = do
    start <- getCurrentTime
    putStrLn $ show (lastTenDigits)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

lastTenDigits :: String
lastTenDigits = reverse $ (take 10 . reverse . show) seriesTotal

seriesTotal :: Integer
seriesTotal = sum [x ^ x | x <- [1..1000]]