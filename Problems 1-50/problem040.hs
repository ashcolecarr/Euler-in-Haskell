-- Project Euler Problem 40
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

main = do
    start <- getCurrentTime
    putStrLn $ show (digitProduct)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

decimalFraction :: String
decimalFraction = '.' : foldr (\x acc -> show x ++ acc) "" [1..]

getNthElement :: Int -> Int
getNthElement element = digitToInt $ head $ drop element decimalFraction

digitProduct :: Int
digitProduct = getNthElement 1 * getNthElement 10 * getNthElement 100 * getNthElement 1000 * getNthElement 10000 * getNthElement 100000 * getNthElement 1000000