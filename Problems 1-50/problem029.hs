-- Project Euler Problem 29
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (nub)

main = do
    start <- getCurrentTime
    putStrLn $ show (length $ nub $ powerSequence)
    stop <- getCurrentTime
    putStrLn $ "Program execution took " ++ show (diffUTCTime stop start) ++ " seconds."

maxNumber :: Integer
maxNumber = 100

powerSequence :: [Integer]
powerSequence = [a ^ b | a <- [2..maxNumber], b <- [2..maxNumber]]