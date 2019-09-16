-- Project Euler Problem 34
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Char (digitToInt)

main = do
    start <- getCurrentTime
    putStrLn $ show (digitFactorialSum)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

-- Set upper bound to 9! + 8! + 7! + 6! + 5! + 4! + 3! + 2! + 1! since each individual digit is a factorial.
digitFactorialSum :: Int
digitFactorialSum = sum ([fac | fac <- [3..upperBound], sum (map (\x -> product [1..x]) $ map (digitToInt) $ show fac) == fac])
    where
        upperBound :: Int
        upperBound = 409113